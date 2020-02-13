module Hedwig (
  module Exports, 
  (#>), withAff,  
  (<#), withAffFlip, 
  (#!>), withEffect, 
  (<!#), withEffectFlip,
  (:>), withAffs, 
  (<:), withAffsFlip, 
  (:!>), withEffects, 
  (<!:), withEffectsFlip, 
  sync
) where

import Data.Tuple (Tuple(..))
import Effect.Aff as Aff
import Prelude

import Hedwig.Application (Init, Update, View, Application, Sub, Sink, mount) as Exports

import Hedwig.Event (
  onClick,
  onDoubleClick,
  onMouseDown,
  onMouseUp,
  onMouseEnter,
  onMouseLeave,
  onMouseOver,
  onMouseOut,
  onInput,
  onCheck,
  onSubmit,
  onBlur,
  onFocus
) as Exports

import Hedwig.Foreign (
  Html,
  Trait,
  attribute,
  element,
  key,
  lazy,
  log,
  on,
  property,
  text,
  transition,
  transition',
  transitionGroup,
  transitionGroup'
) as Exports

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

--- DO NOT EDIT BELOW ---

import Hedwig.Element (
  a,
  abbr,
  address,
  article,
  aside,
  audio,
  b,
  bdi,
  bdo,
  blockquote,
  br,
  button,
  canvas,
  caption,
  cite,
  code,
  col,
  colgroup,
  datalist,
  dd,
  del,
  details,
  dfn,
  div,
  dl,
  dt,
  em,
  embed,
  fieldset,
  figcaption,
  figure,
  footer,
  form,
  h1,
  h2,
  h3,
  h4,
  h5,
  h6,
  header,
  hr,
  i,
  iframe,
  img,
  input,
  ins,
  kbd,
  label,
  legend,
  li,
  main,
  map,
  mark,
  math,
  menu,
  menuitem,
  meter,
  nav,
  node,
  object,
  ol,
  optgroup,
  option,
  output,
  p,
  param,
  pre,
  progress,
  q,
  rp,
  rt,
  ruby,
  s,
  samp,
  section,
  select,
  small,
  source,
  span,
  strong,
  sub,
  summary,
  sup,
  table,
  tbody,
  td,
  textarea,
  tfoot,
  th,
  thead,
  time,
  tr,
  track,
  u,
  ul,
  var,
  video,
  wbr
) as Exports

import Hedwig.Property (
  accept,
  acceptCharset,
  accessKey,
  action,
  align,
  alt,
  autocomplete,
  autofocus,
  autoplay,
  checked,
  class',
  classList,
  cols,
  colspan,
  contentEditable,
  contextmenu,
  controls,
  coords,
  datetime,
  default,
  dir,
  disabled,
  download,
  downloadAs,
  draggable,
  dropzone,
  enctype,
  for,
  headers,
  height,
  hidden,
  href,
  hreflang,
  id,
  isMap,
  itemprop,
  kind,
  lang,
  list,
  loop,
  manifest,
  max,
  maxlength,
  media,
  method,
  min,
  minlength,
  multiple,
  name,
  noValidate,
  pattern,
  ping,
  placeholder,
  poster,
  preload,
  pubdate,
  readOnly,
  rel,
  required,
  reversed,
  rows,
  rowspan,
  sandbox,
  scope,
  selected,
  shape,
  size,
  spellcheck,
  src,
  srcdoc,
  srclang,
  start,
  step,
  style,
  styles,
  tabindex,
  target,
  title,
  type',
  useMap,
  value,
  width,
  wrap
) as Exports

aff2sub :: forall msg . Aff msg -> Exports.Sub msg
aff2sub aff = \sink -> do
  msg <- aff
  Aff.makeAff $ \cb -> do 
    sink msg
    cb (pure unit)
    pure mempty

effect2sub :: forall msg . Effect msg -> Exports.Sub msg
effect2sub effect = \sink -> liftEffect (effect >>= sink)

withAff :: forall model msg . model -> Aff msg -> Tuple model (Array ((msg -> Effect Unit) -> Aff Unit))
withAff model affs = Tuple model [aff2sub affs]

withAffFlip :: forall model msg . Aff msg -> model -> Tuple model (Array ((msg -> Effect Unit) -> Aff Unit))
withAffFlip = flip withAff

withAffs :: forall model msg . model -> Array (Aff msg) -> Tuple model (Array ((msg -> Effect Unit) -> Aff Unit))
withAffs model affs = Tuple model (map aff2sub affs)

withAffsFlip :: forall model msg . Array (Aff msg) -> model -> Tuple model (Array ((msg -> Effect Unit) -> Aff Unit))
withAffsFlip = flip withAffs

withEffect :: forall model msg . model -> Effect msg -> Tuple model (Array ((msg -> Effect Unit) -> Aff Unit))
withEffect model effect = Tuple model [effect2sub effect]

withEffectFlip :: forall model msg . Effect msg -> model -> Tuple model (Array ((msg -> Effect Unit) -> Aff Unit))
withEffectFlip = flip withEffect

withEffects :: forall model msg . model -> Array (Effect msg) -> Tuple model (Array ((msg -> Effect Unit) -> Aff Unit))
withEffects model effects = Tuple model (map effect2sub effects)

withEffectsFlip :: forall model msg . Array (Effect msg) -> model -> Tuple model (Array ((msg -> Effect Unit) -> Aff Unit))
withEffectsFlip = flip withEffects

infixr 6 withAff as #>
infixr 6 withEffect as #!>
infixr 6 withAffFlip as <#
infixr 6 withEffect as <!#

infixr 6 withAffs as :>
infixr 6 withEffects as :!>
infixr 6 withAffsFlip as <:
infixr 6 withEffectsFlip as <!:

sync :: forall a. Effect a -> Aff a
sync = liftEffect
