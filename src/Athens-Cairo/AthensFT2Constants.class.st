"
The various flags from the Freetype/2 header.

The LoadXXXX flags can be used with `primitiveLoadGlyph:flags:` or with the Cairo `primCairoFtFontCreateForFtFace:flags:scale:` primitives.

#### `FT_LOAD_DEFAULT`
  Corresponding to 0, this value is used a default glyph load.  In this
  case, the following will happen:
                                                                        
1. FreeType looks for a bitmap for the glyph corresponding to the
   face's current size.  If one is found, the function returns.  The
   bitmap data can be accessed from the glyph slot (see note below).
                                                                        
2. If no embedded bitmap is searched or found, FreeType looks for a
   scalable outline.  If one is found, it is loaded from the font
   file, scaled to device pixels, then ""hinted"" to the pixel grid in
   order to optimize it.  The outline data can be accessed from the
   glyph slot (see note below).
                                                                        
Note that by default, the glyph loader doesn't render outlines into
bitmaps. The following flags are used to modify this default
behaviour to more specific and useful cases.
                                                                        
#### `FT_LOAD_NO_SCALE`
  Don't scale the vector outline being loaded to 26.6 fractional
  pixels, but kept in font units.  Note that this also disables
  hinting and the loading of embedded bitmaps.  You should only use it
  when you want to retrieve the original glyph outlines in font units.
                                                                        
#### `FT_LOAD_NO_HINTING`
  Don't hint glyph outlines after their scaling to device pixels.
  This generally generates ""blurrier"" glyphs in anti-aliased modes.
                                                                        
  This flag is ignored if `FT_LOAD_NO_SCALE` is set.
                                                                        
#### `FT_LOAD_RENDER` 
  Render the glyph outline immediately into a bitmap before the glyph
  loader returns.  By default, the glyph is rendered for the
  `FT_RENDER_MODE_NORMAL` mode, which corresponds to 8-bit anti-aliased
  bitmaps using 256 opacity levels.  You can use either
  `FT_LOAD_TARGET_MONO` or `FT_LOAD_MONOCHROME` to render 1-bit
  monochrome bitmaps.
                                                                        
  This flag is ignored if `FT_LOAD_NO_SCALE` is set.
                                                                        
####`FT_LOAD_NO_BITMAP`
  Don't look for bitmaps when loading the glyph.  Only scalable
  outlines will be loaded when available, and scaled, hinted, or
  rendered depending on other bit flags.
                                                                        
  This does not prevent you from rendering outlines to bitmaps
  with `FT_LOAD_RENDER`, however.
                                                                        
####`FT_LOAD_VERTICAL_LAYOUT`
  Prepare the glyph image for vertical text layout.  This basically
  means that `face.glyph.advance` will correspond to the vertical
  advance height (instead of the default horizontal advance width),
  and that the glyph image will be translated to match the vertical
  bearings positions.
                                                                        
####`FT_LOAD_FORCE_AUTOHINT`
  Force the use of the FreeType auto-hinter when a glyph outline is
  loaded.  You shouldn't need this in a typical application, since it
  is mostly used to experiment with its algorithm.
                                                                        
####`FT_LOAD_CROP_BITMAP`
  Indicates that the glyph loader should try to crop the bitmap (i.e.,
  remove all space around its black bits) when loading it.  This is
  only useful when loading embedded bitmaps in certain fonts, since
  bitmaps rendered with `FT_LOAD_RENDER` are always cropped by default.
                                                                        
####`FT_LOAD_PEDANTIC`
  Indicates that the glyph loader should perform pedantic
  verifications during glyph loading, rejecting invalid fonts.  This
  is mostly used to detect broken glyphs in fonts.  By default,
  FreeType tries to handle broken fonts also.
                                                                        
#### `FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH`
  Indicates that the glyph loader should ignore the global advance
  width defined in the font.  As far as we know, this is only used by
  the X-TrueType font server, in order to deal correctly with the
  incorrect metrics contained in DynaLab's TrueType CJK fonts.
                                                                        
####`FT_LOAD_NO_RECURSE`
  This flag is only used internally.  It merely indicates that the
  glyph loader should not load composite glyphs recursively.  Instead,
  it should set the `num_subglyph` and `subglyphs` values of the glyph
  slot accordingly, and set ""glyph->format"" to `FT_GLYPH_FORMAT_COMPOSITE`.
                                                                        
  The description of sub-glyphs is not available to client
  applications for now.
                                                                        
####`FT_LOAD_IGNORE_TRANSFORM`
  Indicates that the glyph loader should not try to transform the
  loaded glyph image.  This doesn't prevent scaling, hinting, or
  rendering.
                                                                        
####`FT_LOAD_MONOCHROME`
  This flag is used with `FT_LOAD_RENDER` to indicate that you want
  to render a 1-bit monochrome glyph bitmap from a vectorial outline.
                                                                        
  Note that this has no effect on the hinting algorithm used by the
  glyph loader.  You should better use `FT_LOAD_TARGET_MONO` if you
  want to render monochrome-optimized glyph images instead.
                                                                        
####`FT_LOAD_LINEAR_DESIGN`
  Return the linearly scaled metrics expressed in original font units
  instead of the default 16.16 pixel values.
                                                                        
####`FT_LOAD_NO_AUTOHINT`
  Indicates that the auto-hinter should never be used to hint glyph
  outlines.  This doesn't prevent native format-specific hinters from
  being used.  This can be important for certain fonts where unhinted
  output is better than auto-hinted one.

One of following flags (as LoadTargetXXX) can be used to further specify the result.

####`FT_RENDER_MODE_NORMAL`                                          
     This is the default render mode; it corresponds to 8-bit        
     anti-aliased bitmaps, using 256 levels of opacity.              
                                                                     
####`FT_RENDER_MODE_LIGHT`                                           
     This is similar to `FT_RENDER_MODE_NORMAL`, except that this     
     changes the hinting to prevent stem width quantization.  This   
     results in glyph shapes that are more similar to the original,  
     while being a bit more fuzzy (""better shapes"", instead of       
     ""better contrast"" if you want :-).                              
                                                                     
####`FT_RENDER_MODE_MONO`                                            
     This mode corresponds to 1-bit bitmaps.                         
                                                                     
####`FT_RENDER_MODE_LCD`                                             
     This mode corresponds to horizontal RGB/BGR sub-pixel displays, 
     like LCD-screens.  It produces 8-bit bitmaps that are 3 times   
     the width of the original glyph outline in pixels, and which use
     the `FT_PIXEL_MODE_LCD` mode.                                    
                                                                     
####`FT_RENDER_MODE_LCD_V`                                         
     This mode corresponds to vertical RGB/BGR sub-pixel displays    
     (like PDA screens, rotated LCD displays, etc.).  It produces    
     8-bit bitmaps that are 3 times the height of the original       
     glyph outline in pixels and use the `FT_PIXEL_MODE_LCD_V` mode.  
                                                                     
####Note                                                              
  The LCD-optimized glyph bitmaps produced by `FT_Render_Glyph` are    
  _not filtered_ to reduce color-fringes.  It is up to the caller to 
  perform this pass.                                                 


"
Class {
	#name : #AthensFT2Constants,
	#superclass : #SharedPool,
	#classVars : [
		'LoadCropBitmap',
		'LoadDefault',
		'LoadForceAutohint',
		'LoadIgnoreGlobalAdvanceWidth',
		'LoadIgnoreTransform',
		'LoadLinearDesign',
		'LoadMonochrome',
		'LoadNoAutohint',
		'LoadNoBitmap',
		'LoadNoHinting',
		'LoadNoRecurse',
		'LoadNoScale',
		'LoadPedantic',
		'LoadRender',
		'LoadSbitsOnly',
		'LoadTargetLCD',
		'LoadTargetLCDV',
		'LoadTargetLight',
		'LoadTargetMono',
		'LoadTargetNormal',
		'LoadVerticalLayout',
		'PixelModeGray',
		'PixelModeGray2',
		'PixelModeGray4',
		'PixelModeLCD',
		'PixelModeLCDV',
		'PixelModeMono',
		'PixelModeNone',
		'RenderModeLCD',
		'RenderModeLCDV',
		'RenderModeLight',
		'RenderModeMono',
		'RenderModeNormal',
		'StyleFlagBold',
		'StyleFlagItalic'
	],
	#category : #'Athens-Cairo-Text'
}

{ #category : #'class initialization' }
AthensFT2Constants class >> initialize [
	"FT2Constants initialize"

	LoadDefault := 0.
	LoadNoScale := 1.
	LoadNoHinting := 2.
	LoadRender := 4.
	LoadNoBitmap := 8.
	LoadVerticalLayout := 16.
	LoadForceAutohint := 32.
	LoadCropBitmap := 64.
	LoadPedantic := 128.
	LoadIgnoreGlobalAdvanceWidth := 512.
	LoadNoRecurse := 1024.
	LoadIgnoreTransform := 2048.
	LoadMonochrome := 4096.
	LoadLinearDesign := 8192.
	LoadSbitsOnly := 16384.
	LoadNoAutohint := 32768.

	"One of these flags may be OR'd with the above."
	LoadTargetNormal := 0.
	LoadTargetLight := 1 bitShift: 16.
	LoadTargetMono := 2 bitShift: 16.
	LoadTargetLCD := 3 bitShift: 16.
	LoadTargetLCDV  := 4 bitShift: 16.

	"rendering mode constants"
	RenderModeNormal := 0.
	RenderModeLight := 1.
	RenderModeMono := 2.
	RenderModeLCD := 3.
	RenderModeLCDV := 4.

	"pixel mode constants"
	PixelModeNone := 0.
	PixelModeMono := 1.
	PixelModeGray := 2.
	PixelModeGray2 := 3.
	PixelModeGray4 := 4.
	PixelModeLCD := 5.
	PixelModeLCDV := 6.

	StyleFlagItalic := 1.
	StyleFlagBold := 2
]
