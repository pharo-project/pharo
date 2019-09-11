Class {
	#name : #RGClassDefinitionCompiler,
	#superclass : #Object,
	#instVars : [
		'environment',
		'definitionString',
		'requestor'
	],
	#category : #'Calypso-Ring'
}

{ #category : #'instance creation' }
RGClassDefinitionCompiler class >> on: anRGEnvironment [ 
	^self new 
		environment: anRGEnvironment 
]

{ #category : #accessing }
RGClassDefinitionCompiler >> definitionString [
	^ definitionString
]

{ #category : #accessing }
RGClassDefinitionCompiler >> definitionString: anObject [
	definitionString := anObject
]

{ #category : #accessing }
RGClassDefinitionCompiler >> environment [
	^ environment
]

{ #category : #accessing }
RGClassDefinitionCompiler >> environment: anObject [
	environment := anObject
]

{ #category : #'public access' }
RGClassDefinitionCompiler >> evaluate [
	
	| importer class createdClass |
	
	importer := RGChunkImporter new.
	
	importer fileInFrom: (definitionString, '!') readStream.
	
	class := importer environment ask behaviors detect: [:each | 
		each isRingResolved and: [each superclass isRingResolved ]].

	importer := RGChunkImporter new.
	importer environment: environment.
	importer fileInFrom: (definitionString, '!') readStream.
	
	createdClass := environment ask behaviorNamed: class name.

	createdClass superclass metaclass metaclass isRingResolved ifTrue: [ 
		environment cleanWithMetaclassNamed: createdClass superclass metaclass metaclass name
		].
	^createdClass
]

{ #category : #'as yet unclassified' }
RGClassDefinitionCompiler >> logged: aBoolean [
]

{ #category : #'as yet unclassified' }
RGClassDefinitionCompiler >> new [
	^self
]

{ #category : #'as yet unclassified' }
RGClassDefinitionCompiler >> requestor: anObject [ 
	requestor := anObject
]

{ #category : #'as yet unclassified' }
RGClassDefinitionCompiler >> source: aString [ 
	definitionString := aString
]
