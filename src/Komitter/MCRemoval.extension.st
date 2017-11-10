Extension { #name : #MCRemoval }

{ #category : #'*Komitter-UI' }
MCRemoval >> isMethodPatch [

	^ definition isMethodDefinition
]

{ #category : #'*Komitter-UI' }
MCRemoval >> koClass [
	| klass |
	klass := definition koClass.
	klass ifNil: [ ^ nil ].
	definition addOperation: self on: klass.
	self isClassPatch
		ifTrue: [ klass removed: true ].
	^ klass
]

{ #category : #'*Komitter-UI' }
MCRemoval >> koDefinition [
	
	^  (KomitDefinition definition: self definition)
		operation: self;
		removed: true;
		yourself
]

{ #category : #'*Komitter-UI' }
MCRemoval >> koDestinationText [

	^ ''
]

{ #category : #'*Komitter-UI' }
MCRemoval >> koMethod [
	
	^ (KomitMethod method: self definition)
		operation: self;
		removed: true;
		yourself
]

{ #category : #'*Komitter-UI' }
MCRemoval >> koSourceText [
	^ definition koSourceText 
]

{ #category : #'*Komitter-UI' }
MCRemoval >> selector [

	^ definition selector
]
