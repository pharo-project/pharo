private
addGraphicalHandleFrom: formKey at: aPoint
	"Add the supplied form as a graphical handle centered at the given point.  Return the handle."
	| handle aForm |
	aForm := Smalltalk ui icons
		iconNamed: formKey 
		ifNone: [ Smalltalk ui icons iconNamed: #solidMenuIcon ].
	handle := ImageMorph new form: aForm; bounds: (Rectangle center: aPoint extent: aForm extent).
	handle wantsYellowButtonMenu: false.
	self addMorph: handle.
	handle on: #mouseUp send: #endInteraction to: self.
	^ handle
