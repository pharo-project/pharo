as yet unclassified
testCycle

	| myObject myArray myArray2 pointingObjects |
	myObject := Object new.
	myArray := {myObject . myObject}.
	myArray2 := {myObject . myArray}.

	pointingObjects := (myObject pointersTo) asArray.
	self assert: pointingObjects size = 2.
	self assert: (pointingObjects includesAll: {myArray . myArray2}).
	
	"PointerFinder loops in presence of cycles"
"	myArray at: 1 put: myArray.
	pointingObjects := (PointerFinder pointersTo: myObject) asArray.
	self assert: pointingObjects = {myArray}.
"