Class {
	#name : #HEExportedMetaclass,
	#superclass : #HESymbol,
	#category : 'Hermes-Model'
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
	value := aMetaclass baseClass name.
]
