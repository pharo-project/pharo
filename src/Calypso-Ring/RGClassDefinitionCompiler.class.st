Class {
	#name : 'RGClassDefinitionCompiler',
	#superclass : 'Object',
	#instVars : [
		'environment',
		'definitionString',
		'requestor'
	],
	#category : 'Calypso-Ring',
	#package : 'Calypso-Ring'
}

{ #category : 'instance creation' }
RGClassDefinitionCompiler class >> on: anRGEnvironment [
	^self new
		environment: anRGEnvironment
]

{ #category : 'accessing' }
RGClassDefinitionCompiler >> definitionString [
	^ definitionString
]

{ #category : 'accessing' }
RGClassDefinitionCompiler >> definitionString: anObject [
	definitionString := anObject
]

{ #category : 'accessing' }
RGClassDefinitionCompiler >> environment [
	^ environment
]

{ #category : 'accessing' }
RGClassDefinitionCompiler >> environment: anObject [
	environment := anObject
]

{ #category : 'public access' }
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

{ #category : 'public access' }
RGClassDefinitionCompiler >> logged: aBoolean [
]

{ #category : 'instance creation' }
RGClassDefinitionCompiler >> new [
	^self
]

{ #category : 'public access' }
RGClassDefinitionCompiler >> requestor: anObject [
	requestor := anObject
]

{ #category : 'public access' }
RGClassDefinitionCompiler >> source: aString [
	definitionString := aString
]
