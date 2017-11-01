Extension { #name : #MCModification }

{ #category : #'*Komitter-UI' }
MCModification >> isMethodPatch [

	^ obsoletion isMethodDefinition
]

{ #category : #'*Komitter-UI' }
MCModification >> koClass [
	| klass |
	klass := obsoletion koClass.
	klass ifNil: [ ^ nil ].
	obsoletion addOperation: self on: klass.
	self isClassPatch
		ifTrue: [ klass modified: true ].
	^ klass
]

{ #category : #'*Komitter-UI' }
MCModification >> koDefinition [
	
	^  (KomitDefinition definition: self definition)
		operation: self;
		modified: true;
		yourself
]

{ #category : #'*Komitter-UI' }
MCModification >> koDestinationText [

	^ modification koDestinationText
]

{ #category : #'*Komitter-UI' }
MCModification >> koMethod [
	
	^ (KomitMethod method: self definition)
		operation: self;
		modified: true;
		yourself
]

{ #category : #'*Komitter-UI' }
MCModification >> koSourceText [

	^ obsoletion koSourceText
]

{ #category : #'*Komitter-UI' }
MCModification >> selector [

	^ obsoletion selector
]
