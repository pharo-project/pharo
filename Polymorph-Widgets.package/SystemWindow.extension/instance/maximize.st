*Polymorph-Widgets
maximize
	"Maximise the receiver. If collapsed the uncollapse first."

	self isMinimized ifTrue: [self collapseOrExpand].
	self isMaximized ifFalse: [self expandBoxHit]