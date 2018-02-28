"
I represent a Metaclass
I am stored as a symbol.

When serialized I get the name of the base class.
When deserialized I find the metaclass in the target environment
"
Class {
	#name : #HEExportedMetaclass,
	#superclass : #HESymbol,
	#category : #'Hermes-Model'
}

{ #category : #accessing }
HEExportedMetaclass class >> tag [
	^ 16
]

{ #category : #converting }
HEExportedMetaclass >> asLiteralIn: env [
	^ (env classNamed: value) class
]

{ #category : #accessing }
HEExportedMetaclass >> value: aMetaclass [
	value := aMetaclass instanceSide name.
]
