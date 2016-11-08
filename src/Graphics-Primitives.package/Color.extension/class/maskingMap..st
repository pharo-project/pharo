*Graphics-Primitives
maskingMap: depth 
	"Return a color map that maps all colors except transparent to words of all ones. Used to create a mask for a Form whose transparent pixel value is zero. Cache the most recently used map."
	| sizeNeeded |
	depth <= 8 
		ifTrue: [ sizeNeeded := 1 bitShift: depth ]
		ifFalse: [ sizeNeeded := 4096 ].
	(MaskingMap == nil or: [ MaskingMap size ~= sizeNeeded ]) ifTrue: 
		[ MaskingMap := Bitmap 
			new: sizeNeeded
			withAll: 4294967295.
		MaskingMap 
			at: 1
			put: 0	"transparent" ].
	^ MaskingMap