"
I'm a simple test resource for test purposes
"
Class {
	#name : 'SimpleTestResourceA',
	#superclass : 'SimpleTestResource',
	#category : 'SUnit-Tests-Resources',
	#package : 'SUnit-Tests',
	#tag : 'Resources'
}

{ #category : 'accessing' }
SimpleTestResourceA class >> resources [
	^Array with: SimpleTestResourceA1 with: SimpleTestResourceA2
]
