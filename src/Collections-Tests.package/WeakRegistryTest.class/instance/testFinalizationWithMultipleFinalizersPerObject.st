tests
testFinalizationWithMultipleFinalizersPerObject

	| object registry counter |
	registry := WeakRegistry new.
	object := Object new.
	counter := 0.
	5 timesRepeat: [
		registry add: object executor: (ObjectFinalizer
			receiver: [ counter := counter + 1 ]
			selector: #value) ].
	self assert: registry size = 1.
	object := nil. 
	Smalltalk garbageCollect.
	registry finalizeValues.
	self assert: registry isEmpty.
	self assert: counter = 5
