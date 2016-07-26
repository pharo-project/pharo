copying
copy: aRect
 	"Return a new ColorForm containing the portion of the receiver delineated by aRect."

	| newForm |
	newForm := self class extent: aRect extent depth: depth.
	((BitBlt
		destForm: newForm
		sourceForm: self
		fillColor: nil
		combinationRule: Form over
		destOrigin: 0@0
		sourceOrigin: aRect origin
		extent: aRect extent
		clipRect: newForm boundingBox)
		colorMap: nil) copyBits.
	colors ifNotNil: [newForm colors: colors copy].
	^ newForm
