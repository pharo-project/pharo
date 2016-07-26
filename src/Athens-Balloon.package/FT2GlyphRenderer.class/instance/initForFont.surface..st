initailize-release
initForFont: aFont surface: aSurface

	self assert: (aFont class == FreeTypeFont).
	
	surface := aSurface.
	font := aFont.
	slot := SimpleTextGlyphInfo new.
	
	pixelSize := font pixelSize rounded.

	blt := BitBlt toForm: surface form.

	self loadSurfaceTransform.

	colorMap := Bitmap new: 256.
	
	blt 
		sourceForm: form;
		sourceX: 0;
		sourceY: 0;
		destOrigin: 0@0;
		sourceOrigin: 0@0;
		halftoneForm: nil;
		combinationRule: 24;
		width: form width;
		height: form height;
		colorMap: colorMap;
		clipRect: surface clipRect.
	
	translation := IntegerArray new: 2.
