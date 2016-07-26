as yet unclassified
testInsertingObject
	| theObject  newPosition |
	theObject := Object new.
	newPosition := (text startPosition moveRight:5) insertObject: theObject.
	
	self assert: (newPosition moveLeft: 5) isAtStart not.
	self assert: (newPosition moveLeft: 6) isAtStart.
	

	