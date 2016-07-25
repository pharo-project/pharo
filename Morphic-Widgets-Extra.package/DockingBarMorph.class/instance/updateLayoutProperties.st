private - layout
updateLayoutProperties
	"private - update the layout properties based on adhering,  
	fillsOwner and avoidVisibleBordersAtEdge preferencs"

	(self isHorizontal or: [self isFloating])
		ifTrue: [self listDirection: #leftToRight]
		ifFalse: [self listDirection: #topToBottom].

	self hResizing: #shrinkWrap.
	self vResizing: #shrinkWrap.
	self fillsOwner ifTrue: [
			self isHorizontal ifTrue: [self hResizing: #spaceFill].
			self isVertical ifTrue: [self vResizing: #spaceFill]].
	