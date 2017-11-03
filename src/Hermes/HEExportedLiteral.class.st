Class {
	#name : #HEExportedLiteral,
	#superclass : #HEAbstractExported,
	#instVars : [
		'value'
	],
	#category : #Hermes-Model
}

{ #category : #'instance creation' }
HEExportedLiteral class >> checkAllTags [
	self assert: (self allSubclasses size = (self allSubclasses collect:#tag as:Set) size)
]

{ #category : #'instance creation' }
HEExportedLiteral class >> for: aValue [
	^ self new
		value: aValue;
		yourself
]

{ #category : #'instance creation' }
HEExportedLiteral class >> missingTags [
	^ self allSubclasses reject: [ :e | e class canUnderstand: #tag ]
]

{ #category : #'instance creation' }
HEExportedLiteral class >> nextTag [
	^(self allSubclasses
		collect: [ :e | 
			(e class canUnderstand: #tag)
				ifTrue: [ e tag ]
				ifFalse: [ -1 ] ]) max + 1
]

{ #category : #'instance creation' }
HEExportedLiteral class >> readFrom: aReader [
	| tag subclass |
	tag := aReader readByte.
	subclass := self allSubclasses detect: [ :e | e tag = tag ].
	^ subclass new
		doReadFrom: aReader;
		yourself
]

{ #category : #reading }
HEExportedLiteral >> doReadFrom: aReader [
	self subclassResponsibility.
]

{ #category : #writing }
HEExportedLiteral >> doWriteInto: aWriter [
	self subclassResponsibility.
]

{ #category : #accessing }
HEExportedLiteral >> value [
	^ value
]

{ #category : #accessing }
HEExportedLiteral >> value: anObject [
	value := anObject
]

{ #category : #writing }
HEExportedLiteral >> writeInto: aWriter [
	aWriter writeByte: self class tag.
	self doWriteInto: aWriter.
]
