"
An instance of FLHeader is serialized together with the orignal object graph to serialize. This header is useful to:

1) Attach additional state or metadata associated to the graph being serialized.
2) Attach pre and post materialization actions
"
Class {
	#name : #FLHeader,
	#superclass : #Object,
	#instVars : [
		'preMaterializationActions',
		'additionalObjects',
		'postMaterializationActions',
		'materialization'
	],
	#category : #'Fuel-Core'
}

{ #category : #accessing }
FLHeader >> addPostMaterializationAction: aCleanBlockClosure [
	"The closure may (or not) have one parameter and it will be the materialization."
	aCleanBlockClosure isClean ifFalse: [ self error: 'Post materializaton actions have to be clean closures. For more details see method BlocKClosure >> #isClean' ].
	postMaterializationActions add: aCleanBlockClosure 
]

{ #category : #accessing }
FLHeader >> addPreMaterializationAction: aCleanBlockClosure [
	aCleanBlockClosure isClean ifFalse: [ self error: 'Pre materializaton actions have to be clean closures. For more details see method BlocKClosure >> #isClean' ].
	preMaterializationActions add: aCleanBlockClosure 
]

{ #category : #accessing }
FLHeader >> additionalObjectAt: key [
	^ additionalObjects at: key
]

{ #category : #accessing }
FLHeader >> at: key putAdditionalObject: anObject [ 
	"This is useful if we want to attach objects to a package that will also be serialized. The way they are stored is key-value."
	additionalObjects at: key put: anObject
]

{ #category : #executing }
FLHeader >> executePostMaterializationActions [
	postMaterializationActions do: [:each | each cull: materialization]
]

{ #category : #executing }
FLHeader >> executePreMaterializationActions [
	preMaterializationActions do: [:each | each value]
]

{ #category : #initialization }
FLHeader >> initialize [
	additionalObjects := IdentityDictionary new.
	preMaterializationActions := OrderedCollection new.
	postMaterializationActions := OrderedCollection new.
]

{ #category : #testing }
FLHeader >> isEmpty [
	^ preMaterializationActions isEmpty 
		and: [ postMaterializationActions isEmpty
		and: [ additionalObjects isEmpty ] ]
]

{ #category : #accessing }
FLHeader >> materialization: aMaterialization [
	"This is just set once the materialization had happened"
	materialization := aMaterialization 
]

{ #category : #accessing }
FLHeader >> postMaterializationActions: listOfPostMaterializationActions [
	postMaterializationActions := listOfPostMaterializationActions.
	
]

{ #category : #accessing }
FLHeader >> preMaterializationActions: listOfPreMaterializationActions [
	preMaterializationActions := listOfPreMaterializationActions.
	
]
