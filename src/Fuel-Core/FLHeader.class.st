"
An instance of FLHeader is serialized together with the orignal object graph to serialize. This header is useful to:

1) Attach additional state or metadata associated to the graph being serialized.
2) Attach pre and post materialization actions
"
Class {
	#name : 'FLHeader',
	#superclass : 'Object',
	#traits : 'TFLConfigurable',
	#classTraits : 'TFLConfigurable classTrait',
	#instVars : [
		'preMaterializationActions',
		'additionalObjects',
		'postMaterializationActions'
	],
	#category : 'Fuel-Core-Base',
	#package : 'Fuel-Core',
	#tag : 'Base'
}

{ #category : 'instance creation' }
FLHeader class >> forMaterialization [
	^ self basicNew
		initializeForMaterialization;
		initialize;
		yourself
]

{ #category : 'instance creation' }
FLHeader class >> forSerialization [
	^ self basicNew
		initializeForSerialization;
		initialize;
		yourself
]

{ #category : 'accessing' }
FLHeader >> addPostMaterializationAction: aCleanBlockClosure [
	"The closure receives the header, materialization and the global environment as optional arguments"
	aCleanBlockClosure isClean ifFalse: [ self error: 'Post materializaton actions have to be clean closures. For more details see method BlocKClosure >> #isClean' ].
	postMaterializationActions add: aCleanBlockClosure
]

{ #category : 'accessing' }
FLHeader >> addPreMaterializationAction: aCleanBlockClosure [
	"The closure receives the header and the global environment as optional arguments"
	aCleanBlockClosure isClean ifFalse: [ self error: 'Pre materializaton actions have to be clean closures. For more details see method BlocKClosure >> #isClean' ].
	preMaterializationActions add: aCleanBlockClosure
]

{ #category : 'accessing' }
FLHeader >> additionalObjectAt: key [
	^ additionalObjects at: key
]

{ #category : 'accessing' }
FLHeader >> additionalObjects: aCollection [
	additionalObjects := aCollection
]

{ #category : 'accessing' }
FLHeader >> at: key putAdditionalObject: anObject [ 
	"This is useful if we want to attach objects to a package that will also be serialized. The way they are stored is key-value."
	additionalObjects at: key put: anObject
]

{ #category : 'executing' }
FLHeader >> executePostMaterializationActions [
	postMaterializationActions ifNil: [ ^ self ].
	
	postMaterializationActions do: [ :each |
		each
			cull: self context materializedObjects
			cull: self configuration environment ]
]

{ #category : 'executing' }
FLHeader >> executePreMaterializationActions [
	preMaterializationActions ifNil: [ ^ self ].

	preMaterializationActions do: [ :each |
		each
			cull: self context materializedObjects
			cull: self configuration environment ]
]

{ #category : 'hooks' }
FLHeader >> fuelAccept: aGeneralMapper [
	"The header is actually being serialized. Override the trait"
	self isEmpty
		ifFalse: [ super fuelAccept: aGeneralMapper ]
		ifTrue: [
			aGeneralMapper
				visitSubstitution: self
				by: nil ]
]

{ #category : 'initialization' }
FLHeader >> initializeForMaterialization [
	additionalObjects := Dictionary new.
	preMaterializationActions := #().
	postMaterializationActions := #()
]

{ #category : 'initialization' }
FLHeader >> initializeForSerialization [
	additionalObjects := self configuration additionalObjects.
	preMaterializationActions := self configuration preMaterializationActions.
	postMaterializationActions := self configuration postMaterializationActions
]

{ #category : 'testing' }
FLHeader >> isEmpty [
	^ preMaterializationActions isEmpty 
		and: [ postMaterializationActions isEmpty
		and: [ additionalObjects isEmpty ] ]
]

{ #category : 'accessing' }
FLHeader >> postMaterializationActions: listOfPostMaterializationActions [
	postMaterializationActions := listOfPostMaterializationActions.
]

{ #category : 'accessing' }
FLHeader >> preMaterializationActions: listOfPreMaterializationActions [
	preMaterializationActions := listOfPreMaterializationActions.
]
