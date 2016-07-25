drawing
drawOn: aCanvas in: rect offset: o
	"Draw the highlight on the given canvas for the given rectangle."

	| r |
	r := self fillWidth
		ifTrue: [ rect left @ (self bounds top + rect top) corner: rect right @ (self bounds bottom + rect top) ]
		ifFalse: [ self bounds translateBy: rect topLeft ].
	r := r
		translateBy:
			(self fillWidth
				ifTrue: [ 0 @ o y negated ]
				ifFalse: [ o negated ]).
	self color isTransparent
		ifFalse: [ aCanvas fillRectangle: r color: self color ].
	(self borderWidth > 0 and: [ self borderColor isTransparent not ])
		ifTrue: [ 
			(self borderSides includes: #top)
				ifTrue: [ aCanvas fillRectangle: (r withHeight: self borderWidth) color: self borderColor ].
			(self borderSides includes: #right)
				ifTrue: [ aCanvas fillRectangle: (r withLeft: r right - self borderWidth) color: self borderColor ].
			(self borderSides includes: #bottom)
				ifTrue: [ aCanvas fillRectangle: (r withTop: r bottom - self borderWidth) color: self borderColor ].
			(self borderSides includes: #left)
				ifTrue: [ aCanvas fillRectangle: (r withWidth: self borderWidth) color: self borderColor ] ]