accessing selection
handleSelectionChange: aBlock
	| prevMarkBlock prevPointBlock |
	self flag: 'hack here because text segments are not well designed regarding text editing'.
	self primarySelection ifNil: [ self addPrimarySelection  ].
	prevMarkBlock := self markBlock.
	prevPointBlock := self pointBlock.
	aBlock value.
	(prevMarkBlock ~= self markBlock or: [ prevPointBlock ~= self pointBlock ])
		ifTrue: [ 
			"have to invalidate the full surface because of the selection"
			self invalidRect: (self expandFullBoundsForDropShadow: self drawingBounds).
			self scrollPane ifNotNil: [ :sp | sp selectionChanged ].
			self announce: (RubSelectionChanged from: self previousMarkBlock: prevMarkBlock previousPointBlock: prevPointBlock) ] 