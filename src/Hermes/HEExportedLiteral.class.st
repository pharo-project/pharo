"
I represent all the literals that a method and a trait composition can have.

My subclasses implements all the behavior for the different types of literals. 
All my subclasses has a #tag number. Each tag number should be unique. 
This number is written in the stream before of the content of the literal. 
In this way, the literal can be properly deserialized. 

By sure, playing with the tag numbers requires incrementing the version of the hermes files, 
as they do not keep compatibility if the numbers change.

For deserializing a literal I have a class side message called

#readFrom: aHermesStream

This message returns the correct instance representing the literal value.

Also I implement helping classSide methods to check the sanity of the tags, #checkAllTags and #missingTags

By default I have a value, but my subclasses can have more instance variables.

My subclasses implements the method 
#asLiteralIn: env
This method creates the proper literal in the given environment. Some of the literals are environment dependent,
for example the classes.
"
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

{ #category : #converting }
HEExportedLiteral >> asExportedLiteral [
	"I am already an exported literal, I do not need any conversion"
	^ self
]

{ #category : #reading }
HEExportedLiteral >> doReadFrom: aReader [
	"Main template method to read the instance from the reader stream"
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
