"
I am an abstract class defining the API that should be understood by any command or command group.
"
Class {
	#name : 'CmObject',
	#superclass : 'Object',
	#category : 'Commander2-Core',
	#package : 'Commander2',
	#tag : 'Core'
}

{ #category : 'testing' }
CmObject >> isCommand [
	^ false
]

{ #category : 'testing' }
CmObject >> isGroup [
	^ false
]
