initialization
initialize
	| w |
	super initialize.
	w := Display width > 2048 ifTrue: [ 4096 ] ifFalse: [ 2048 ].
	externals := OrderedCollection new: 100.
	span := Bitmap new: w.
	bitBlt := nil.
	self bitBlt: ((BitBlt toForm: Display) destRect: Display boundingBox; yourself).
	forms := #().
	deferred := false.