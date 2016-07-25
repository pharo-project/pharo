as yet unclassified
testNoPointingObject

	| myObject |
	myObject := Object new.
	self assert: (myObject pointersTo) isEmpty