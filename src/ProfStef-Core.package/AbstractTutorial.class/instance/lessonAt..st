tutorial
lessonAt: anInteger
	| lessonSelector |
	lessonSelector := self tutorial at: anInteger.
	^ self perform: lessonSelector.