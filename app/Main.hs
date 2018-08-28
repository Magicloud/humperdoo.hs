{-# LANGUAGE LambdaCase, OverloadedLabels, OverloadedLists, OverloadedStrings,
             ScopedTypeVariables, UnicodeSyntax #-}
module Main where

-- import           Control.Applicative.Unicode
-- import           Control.Arrow.Unicode
-- import qualified Control.Category.Unicode    as Category
import           Control.Monad
import           Control.Monad.Unicode
import           Data.Bool.Unicode
import           Data.Eq.Unicode
import           Data.List.Split
-- import           Data.Foldable.Unicode
import           Data.Function.Unicode
import           Data.GI.Base.GValue
import           Data.IORef
import           Data.List
import           Data.List.Unicode
import           Data.Maybe
-- import           Data.Monoid.Unicode
-- import           Data.Ord.Unicode
import           Data.Text             as Text ( Text, pack, unpack )
import           GHC.Int
import           GI.Gdk
import           GI.GdkPixbuf
import           GI.Gio
import qualified GI.Gtk                as Gtk
import           GI.Pango
import           Paths_humperdoo
import           Prelude.Unicode
import           System.Directory
import           System.FilePath
import           System.Process

data Conf = Conf { confCurrFolder ∷ FilePath
                 , confBuilder ∷ Gtk.Builder
                 , confIconTheme ∷ Gtk.IconTheme }

iconSize ∷ GHC.Int.Int32
iconSize = 24

blankIcon ∷ IO Pixbuf
blankIcon = pixbufNew ColorspaceRgb True 8 iconSize iconSize ≫= \case
  Just pb → return pb
  Nothing → error "not enough memory to allocate"

getGtkObj ∷ GObject o' ⇒ Gtk.Builder → Text → (ManagedPtr o' → o') → IO o'
getGtkObj builder name gtkType =
  #getObject builder name ≫= \case
    Nothing → error $ "no such object as " ⧺ unpack name
    Just o → Gtk.castTo gtkType o ≫= \case
      Nothing → error "object is not as type"
      Just o_ → return o_

setupBuilder ∷ IO Gtk.Builder
setupBuilder = getDataFileName "humperdoo.glade" ≫= Gtk.builderNewFromFile ∘ pack

fallbackIcon ∷ IORef Conf → FilePath → IO Pixbuf
fallbackIcon c fp =
  doesDirectoryExist fp ≫= \case
    False → blankIcon
    True → do
      iconTheme ← confIconTheme <$> readIORef c
      #loadIcon iconTheme "folder" iconSize [] ≫= \case
        Nothing → blankIcon
        Just i → return i

filePath2Icon ∷ IORef Conf → String → IO Pixbuf
filePath2Icon c fp = do
  (ct, ok) ← contentTypeGuess (Just $ pack fp) Nothing
  if ok
     then do
       iconTheme ← confIconTheme <$> readIORef c
       icon ← contentTypeGetIcon ct
       #lookupByGicon iconTheme icon iconSize [] ≫= \case
         Just iconInfo → #loadIcon iconInfo
         Nothing → fallbackIcon c fp
     else fallbackIcon c fp

gtypePixbuf ∷ IO GType
gtypePixbuf = blankIcon ≫= gobjectType

hidden ∷ [FilePath] → [FilePath]
hidden = filter ((≠) '.' ∘ head)

folder1st ∷ [(FilePath, Bool)] → [(FilePath, Bool)]
folder1st = sortBy (\(a, aIsDir) (b, bIsDir) →
  if aIsDir == bIsDir
     then compare (takeFileName a) (takeFileName b)
     else if aIsDir
              then LT
              else GT)

detachDir ∷ [(FilePath, Bool)] → [FilePath]
detachDir = map fst

attachDir ∷ [FilePath] → IO [(FilePath, Bool)]
attachDir = mapM (\a → do
  aIsDir ← doesDirectoryExist a
  return (a, aIsDir))

enterFolder ∷ IORef Conf → FilePath → IO ()
enterFolder c folder = do
  modifyIORef c $ \conf → conf { confCurrFolder = folder }
  builder ← confBuilder <$> readIORef c
  folderItems ← listDirectory folder ≫= fmap (detachDir ∘ folder1st) ∘ attachDir ∘ map (folder </>) ∘ hidden -- TODO inotify the folder
  lsFolderEntries ← getGtkObj builder "lsFolderEntries" Gtk.ListStore
  #clear lsFolderEntries
  mapM_ (\fp → do
    icoPixbuf ← filePath2Icon c fp
    t ← gtypePixbuf
    p ← #append lsFolderEntries

    desc ← toGValue $ Just $ last $ splitPath fp
    #setValue lsFolderEntries p 0 desc

    box ← newGValue t
    withManagedPtr icoPixbuf (set_object box)
    #setValue lsFolderEntries p 1 box

    filePath ← toGValue $ Just fp
    #setValue lsFolderEntries p 2 filePath) folderItems
  txtURI ← getGtkObj builder "txtURI" Gtk.Entry
  set txtURI [ #editable := False ]
  #getWindow txtURI ≫= \case
    Just win → do
      display ← #getDisplay win
      cursor ← cursorNewFromName display "pointer"
      windowSetCursor win cursor
    Nothing → error "No window for txtURI"
  #setText txtURI $ pack $ replace [pathSeparator] [' ', pathSeparator, ' '] folder

iconViewItemActivated ∷ IORef Conf → Gtk.IconView → Gtk.TreePath → IO ()
iconViewItemActivated c w p = do
  model ← fromJust <$> #getModel w
  (_ok, iter) ← #getIter model p
  cf ← confCurrFolder <$> readIORef c
  fp ← #getValue model iter 2 ≫= fmap ((</>) cf ∘ fromJust) ∘ fromGValue
  doesDirectoryExist fp ≫= \case
    True → enterFolder c fp
    False → do
      void $ spawnCommand $ "xdg-open " ⧺ fp
      Gtk.mainQuit

replace ∷ Eq a ⇒ [a] → [a] → [a] → [a]
replace f t s = intercalate t $ splitOn f s

applyTuple2 ∷ (a, a) → (a → b) → (b, b)
applyTuple2 (x, y) f = (f x, f y)

isAtSeperator ∷ Int → String → Bool
isAtSeperator i s = case i of
  0 → True -- space before root
  1 → True -- root
  2 → True -- space after root
  3 → True -- 1st lvl
  _ → any (\j → take 3 (drop j s) ≡ [' ', pathSeparator, ' ']) ([ i - 2, i - 1, i ] ∷ [Int])

main ∷ IO ()
main = do
  initFolder ← getHomeDirectory
  void $ Gtk.init Nothing
  builder ← setupBuilder
  iconTheme ← Gtk.iconThemeGetDefault
  state ← newIORef $ Conf initFolder builder iconTheme

  winMain ← getGtkObj builder "winMain" Gtk.Widget
  #showAll winMain
  void $ Gtk.on winMain #destroy Gtk.mainQuit

  ivFolderEntries ← getGtkObj builder "ivFolderEntries" Gtk.IconView
  #setTextColumn ivFolderEntries 0
  #setPixbufColumn ivFolderEntries 1
  void $ Gtk.on ivFolderEntries #itemActivated $ iconViewItemActivated state ivFolderEntries

  btnOpen ← getGtkObj builder "btnOpen" Gtk.Button
  void $ Gtk.on btnOpen #clicked $ do
    selectedItems ← #getSelectedItems ivFolderEntries
    when ((¬) $ null selectedItems) $
      iconViewItemActivated state ivFolderEntries $ head selectedItems

  txtURI ← getGtkObj builder "txtURI" Gtk.Entry
  void $ Gtk.on txtURI #iconPress $ \_eip eb →
    Gtk.get eb #type ≫= \case
      EventTypeButtonPress → enterFolder state "/"
      _ → return ()
  void $ Gtk.on txtURI #buttonPressEvent $ \eb → do
    Gtk.get eb #type ≫= \case
      EventTypeButtonPress → do
        px ← ceiling <$> Gtk.get eb #x
        py ← ceiling <$> Gtk.get eb #y
        layout ← #getLayout txtURI
        (lx, ly) ← #getLayoutOffsets txtURI
        (isIn, index_, _trailing) ← #xyToIndex layout (lx + px ⋅ SCALE) (ly + py ⋅ SCALE)
        str ← unpack <$> #getText txtURI
        let index = fromIntegral index_
            (p, s) = flip applyTuple2 (replace  [' ', pathSeparator, ' '] [pathSeparator]) $ splitAt index str
            mNextSeparatorAt = elemIndex pathSeparator s
        if isIn
           then if isAtSeperator index str ∨ isNothing mNextSeparatorAt
                   then return ()
                   else enterFolder state $ p ⧺ take (fromJust mNextSeparatorAt) s
             -- If the widget is shorter, hide initialize parts.
           else do
             set txtURI [ #editable := True ]
             #getWindow txtURI ≫= \case
               Just win → windowSetCursor win (Nothing ∷ Maybe Cursor)
               Nothing → error "No window for txtURI"
             confCurrFolder <$> readIORef state ≫= #setText txtURI ∘ pack
      _ → return ()
    return False
  void $ Gtk.on txtURI #activate $
    #getText txtURI ≫= enterFolder state ∘ unpack

  enterFolder state initFolder

  Gtk.main
