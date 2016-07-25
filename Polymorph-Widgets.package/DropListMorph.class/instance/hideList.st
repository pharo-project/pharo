protocol
hideList
	"Hide the list."

	self listMorph ifNil: [^self].
	self listVisible ifFalse: [^self].
	self listMorph delete.
	self roundedCorners: #(1 2 3 4).
	(self buttonMorph ifNil: [^self]) roundedCorners: (self roundedCorners copyWithoutAll: #(1 2)).
	self fillStyle: self fillStyleToUse.
	self wantsKeyboardFocus
		ifTrue: [self takeKeyboardFocus]