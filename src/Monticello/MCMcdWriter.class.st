"
Writing MCD format
"
Class {
	#name : #MCMcdWriter,
	#superclass : #MCMczWriter,
	#category : #'Monticello-Storing'
}

{ #category : #accessing }
MCMcdWriter class >> readerClass [
	^ MCMcdReader
]

{ #category : #writing }
MCMcdWriter >> writeBaseInfo: aVersionInfo [
	| string |
	string := self serializeVersionInfo: aVersionInfo.
	self addString: string at: 'base' encodedTo: 'utf8'.

]

{ #category : #visiting }
MCMcdWriter >> writeDefinitions: aVersion [
	self writeBaseInfo: aVersion baseInfo.
	self writePatch: aVersion patch.
]

{ #category : #writing }
MCMcdWriter >> writeNewDefinitions: aCollection [
	self addString: (self serializeDefinitions: aCollection) at: 'new/source.', self snapshotWriterClass extension encodedTo: 'utf8'.
]

{ #category : #writing }
MCMcdWriter >> writeOldDefinitions: aCollection [
	self addString: (self serializeDefinitions: aCollection) at: 'old/source.', self snapshotWriterClass extension encodedTo: 'utf8'.
]

{ #category : #writing }
MCMcdWriter >> writePatch: aPatch [
	| old new |
	old := OrderedCollection new.
	new := OrderedCollection new.
	aPatch operations do:
		[:ea |
		ea isRemoval ifTrue: [old add: ea definition].
		ea isAddition ifTrue: [new add: ea definition].
		ea isModification ifTrue: [old add: ea baseDefinition. new add: ea definition]].
	self writeOldDefinitions: old.
	self writeNewDefinitions: new.
	self addString: (self serializeInBinary: aPatch) at: 'patch.bin'.
]
