image manipulation
replaceColor: oldColor withColor: newColor
	"Replace one color with another everywhere is this form"

	| cm newInd target ff |
	self depth = 32
		ifTrue: [cm := (Color  cachedColormapFrom: 16 to: 32) copy]
		ifFalse: [cm := Bitmap new: (1 bitShift: (self depth min: 15)).
				1 to: cm size do: [:i | cm at: i put: i - 1]].
	newInd := newColor pixelValueForDepth: self depth.
	cm at: (oldColor pixelValueForDepth: (self depth min: 16))+1 put: newInd.
	target := newColor isTransparent 
		ifTrue: [ff := Form extent: self extent depth: depth.
			ff fillWithColor: newColor.  ff]
		ifFalse: [self].
	(BitBlt toForm: target)
		sourceForm: self;
		sourceOrigin: 0@0;
		combinationRule: Form paint;
		destX: 0 destY: 0 width: width height: height;
		colorMap: cm;
		copyBits.
	newColor = Color transparent 
		ifTrue: [target displayOn: self].