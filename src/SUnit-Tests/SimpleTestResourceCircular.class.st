"
I'm a simple test resource for test purposes
"
Class {
	#name : 'SimpleTestResourceCircular',
	#superclass : 'SimpleTestResource',
	#category : 'SUnit-Tests-Resources',
	#package : 'SUnit-Tests',
	#tag : 'Resources'
}

{ #category : 'accessing' }
SimpleTestResourceCircular class >> resources [

	^ preventAvailability
		ifNil: [ super resources , ( Array with: SimpleTestResourceA1 ) ]
		ifNotNil: [ super resources , ( Array with: SimpleTestResourceA1 with: SimpleTestResourceCircular1 ) ]
]
