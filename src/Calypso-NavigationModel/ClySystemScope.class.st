"
I am a root of hierarchy of scope classes which represent a global point of view on concrete system.

My subclasses define what is the concrete system and implement accessing methods to retrieve all possible information from it.

I am supposed to be created with single object basis. In case of multiple basis I signal error.
So to create me use single basis #of: message: 

	ClyConcreteSystemScope of: aSystem

And to access the system I provide simple message #system which just returns single basis item
"
Class {
	#name : #ClySystemScope,
	#superclass : #ClyTypedScope,
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #private }
ClySystemScope >> asUnifiedInstance [
	^self
]

{ #category : #printing }
ClySystemScope >> description [

	^name ifNil: [self system name]
]

{ #category : #initialization }
ClySystemScope >> prepareBasisFrom: aCollection [
	aCollection size = 1 ifFalse: [ 
		self error: 'System scope should be created on single basis' ].

	^aCollection
]

{ #category : #printing }
ClySystemScope >> printBasisObject: aSystemEnvironment on: aStream [

	aStream nextPutAll: aSystemEnvironment name
]

{ #category : #accessing }
ClySystemScope >> system [
	^basisObjects anyOne
]
