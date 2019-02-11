Class {
	#name : #MetacelloVersionMethodSection,
	#superclass : #MetacelloMethodSection,
	#instVars : [
		'block',
		'versionSpec',
		'parent',
		'methodSections'
	],
	#category : #'Metacello-Core-Constructors'
}

{ #category : #accessing }
MetacelloVersionMethodSection >> addMethodSection: methodSection [

	methodSection parent: self.
	self methodSections add: methodSection
]

{ #category : #accessing }
MetacelloVersionMethodSection >> attributeOrPath [
    ^ self parent == nil
        ifTrue: [ 
            self attributes size == 1
                ifTrue: [ self attributes first ]
                ifFalse: [ self attributes ] ]
        ifFalse: [ self attributePath ]
]

{ #category : #accessing }
MetacelloVersionMethodSection >> attributePath [
    | path |
    path := MetacelloMethodSectionPath with: self attributes.
    ^ self parent == nil
        ifTrue: [ path ]
        ifFalse: [ self parent attributePath , path ]
]

{ #category : #accessing }
MetacelloVersionMethodSection >> block [
	^ block
]

{ #category : #accessing }
MetacelloVersionMethodSection >> block: anObject [
	block := anObject
]

{ #category : #accessing }
MetacelloVersionMethodSection >> methodSections [

	methodSections ifNil: [ methodSections := OrderedCollection new ].
	^methodSections
]

{ #category : #accessing }
MetacelloVersionMethodSection >> parent [
	^ parent
]

{ #category : #accessing }
MetacelloVersionMethodSection >> parent: anObject [
	parent := anObject
]

{ #category : #printing }
MetacelloVersionMethodSection >> printOn: aStream [
  aStream
    nextPutAll: self class name asString;
    nextPut: $(.
  self versionSpec printOn: aStream.
  aStream nextPut: $)
]

{ #category : #accessing }
MetacelloVersionMethodSection >> topParent [
	self parent == nil
		ifTrue: [ ^ self ].
	^ self parent topParent
]

{ #category : #accessing }
MetacelloVersionMethodSection >> versionSpec [
	^ versionSpec
]

{ #category : #accessing }
MetacelloVersionMethodSection >> versionSpec: anObject [
	versionSpec := anObject
]
