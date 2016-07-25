as yet unclassified
testBasic1

	| myObject myArray |
	myObject := Object new.
	myArray := {myObject . myObject}.
	self assert: (myObject pointersTo) asArray = {myArray}