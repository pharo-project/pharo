initialize-release
setController: aECController class: aClass source: aString position: anInteger 
	controller := aECController.
	theClass := aClass.
	source := aString.
	position := anInteger.
	self createRanges.
	self compute