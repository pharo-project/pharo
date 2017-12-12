Class {
	#name : #HEExportedLiteral,
	#superclass : #HEAbstractExported,
	#instVars : [
		'value'
	],
	#category : #'Hermes-Model'
}

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

{ #category : #accessing }
HEExportedLiteral >> value [
	^ value
]

{ #category : #accessing }
HEExportedLiteral >> value: anObject [
	value := anObject
]
