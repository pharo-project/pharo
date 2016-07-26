displaying
maskingMap
	"Return a color map that maps all colors except transparent to words of all ones. Used to create a mask for a Form whose transparent pixel value is zero."
	| maskingMap |
	maskingMap := Bitmap new: (1 bitShift: depth) withAll: 16rFFFFFFFF.
	1 to: colors size do:[:i|
		(colors at: i) isTransparent ifTrue:[maskingMap at: i put: 0].
	].
	colors size+1 to: maskingMap size do:[:i| maskingMap at: i put: 0].
	^maskingMap