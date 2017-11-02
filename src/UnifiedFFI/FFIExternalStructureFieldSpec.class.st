"
I'm a field spec for fields in FFIExternalStructure
"
Class {
	#name : #FFIExternalStructureFieldSpec,
	#superclass : #Object,
	#instVars : [
		'fields'
	],
	#category : 'UnifiedFFI'
}

{ #category : #accessing }
FFIExternalStructureFieldSpec >> addField: aName type: anExternalType [
	fields at: aName put: anExternalType
]

{ #category : #compiling }
FFIExternalStructureFieldSpec >> compileSpec [ 
	"This will compile the structure spec into something ExternalStructure can understand"
	^ fields values 
		inject: WordArray new
		into: [ :all :each | all, (each externalTypeWithArity embeddedSpecWithSize: each externalTypeSize) ]
]

{ #category : #accessing }
FFIExternalStructureFieldSpec >> fieldNames [
	^ fields keys
]

{ #category : #accessing }
FFIExternalStructureFieldSpec >> fieldsAndTypesDo: aBlock [
	^ fields keysAndValuesDo: aBlock
]

{ #category : #initialization }
FFIExternalStructureFieldSpec >> initialize [ 
	super initialize.
	fields := OrderedDictionary new
]

{ #category : #accessing }
FFIExternalStructureFieldSpec >> typeFor: aString [ 
	^ fields at: aString
]
