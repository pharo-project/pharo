"
I'm a simple test ressource for test purposes
"
Class {
	#name : #SimpleTestResourceCircular,
	#superclass : #SimpleTestResource,
	#category : #'SUnit-Tests-Resources'
}

{ #category : #accessing }
SimpleTestResourceCircular class >> resources [
	^preventAvailability isNil
		ifTrue: [super resources , (Array with: SimpleTestResourceA1)]
		ifFalse:
			[super resources
				, (Array with: SimpleTestResourceA1 with: SimpleTestResourceCircular1)]
]
