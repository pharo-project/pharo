Extension { #name : #MCAddition }

{ #category : #'*Komitter-UI' }
MCAddition >> isMethodPatch [

	^ definition isMethodDefinition
]

{ #category : #'*Komitter-UI' }
MCAddition >> koClass [
	| klass |
	klass := definition koClass.
	klass ifNil: [ ^ nil ].
	definition addOperation: self on: klass.
	self isClassPatch
		ifTrue: [ klass added: true ].
	^ klass
]

{ #category : #'*Komitter-UI' }
MCAddition >> koDefinition [
	
	^ (KomitDefinition definition: self definition)
		operation: self;
		added: true;
		yourself
]

{ #category : #'*Komitter-UI' }
MCAddition >> koDestinationText [
	
	^ definition koDestinationText
]

{ #category : #'*Komitter-UI' }
MCAddition >> koMethod [
	
	^ (KomitMethod method: self definition)
		operation: self;
		added: true;
		yourself
]

{ #category : #'*Komitter-UI' }
MCAddition >> koSourceText [
	
	^ ''
]

{ #category : #'*Komitter-UI' }
MCAddition >> selector [

	^ definition selector
]
