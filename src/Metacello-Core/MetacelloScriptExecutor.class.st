"
MetacelloScriptExecutor decodes script args and launches MetacelloScriptEngine to run for each projectSpec encounters...i,e., primarily here to handl array and block args to configuration:, baseline: or project:.
"
Class {
	#name : 'MetacelloScriptExecutor',
	#superclass : 'Object',
	#instVars : [
		'options',
		'results',
		'singleResult',
		'actionArg',
		'baselineArg',
		'classNameArg',
		'repositoryArg'
	],
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'execution' }
MetacelloScriptExecutor >> applyArgsToProjectSpec: aProjectSpec [
    classNameArg ifNotNil: [ aProjectSpec className: classNameArg ].
    repositoryArg ifNotNil: [ aProjectSpec repository: repositoryArg ].
    ^ aProjectSpec
]

{ #category : 'args' }
MetacelloScriptExecutor >> baselineArg [
	^ baselineArg
]

{ #category : 'args' }
MetacelloScriptExecutor >> baselineArg: anObject [
	baselineArg := anObject
]

{ #category : 'configuration' }
MetacelloScriptExecutor >> beStrict: aBoolean [

	self options at: #strict put: aBoolean
]

{ #category : 'args' }
MetacelloScriptExecutor >> classNameArg [
    ^ classNameArg
]

{ #category : 'args' }
MetacelloScriptExecutor >> classNameArg: anObject [
    classNameArg := anObject
]

{ #category : 'execution' }
MetacelloScriptExecutor >> execute [

	baselineArg ifNil: [ self error: 'Baseline not specified' ].
	self executeString: baselineArg do: [ :projectReferenceSpec |
			| engine |
			engine := MetacelloScriptEngine new
				          options: self options copy;
				          projectSpec: projectReferenceSpec;
				          yourself.
			[
				self class codeSupportAnnouncer announce: (MetacelloExecutionStartAnnouncement executor: self).
				engine perform: actionArg key withArguments: actionArg value ] ensure: [
				self class codeSupportAnnouncer announce: (MetacelloExecutionEndAnnouncement executor: self) ].
			engine result ifNotNil: [ :result | self results add: result ] ].
	^ (self singleResult and: [ self results size == 1 ])
		  ifTrue: [ self results first ]
		  ifFalse: [ self results ]
]

{ #category : 'execution callback' }
MetacelloScriptExecutor >> executeString: aString do: projectSpecBlock [

	| projectSpec |
	singleResult ifNil: [ self singleResult: true ].
	projectSpec := MetacelloProjectRegistration projectSpecForClassNamed: aString ifAbsent: [ ].

	(self shouldBeExecutedFor: projectSpec) ifTrue: [ projectSpecBlock value: (self applyArgsToProjectSpec: projectSpec copy) ]
]

{ #category : 'actions api' }
MetacelloScriptExecutor >> fetch: required [
    actionArg := #'fetch:' -> {required}
]

{ #category : 'actions api' }
MetacelloScriptExecutor >> get [
    actionArg := #'get' -> #()
]

{ #category : 'options api' }
MetacelloScriptExecutor >> ignoreImage: aBool [
    self options at: #'ignoreImage' put: aBool
]

{ #category : 'actions api' }
MetacelloScriptExecutor >> list [
  actionArg := #'list' -> #().
  self singleResult: false
]

{ #category : 'actions api' }
MetacelloScriptExecutor >> load: required [
    actionArg := #load: -> {required}
]

{ #category : 'options api' }
MetacelloScriptExecutor >> loader: aLoader [
	
	self options at: #loader put: aLoader
]

{ #category : 'actions api' }
MetacelloScriptExecutor >> lock [
    actionArg := #'lock' -> #()
]

{ #category : 'options api' }
MetacelloScriptExecutor >> onConflict: aBlock [
    self options at: #'onConflict' put: aBlock
]

{ #category : 'options api' }
MetacelloScriptExecutor >> onDowngrade: aBlock [
    self options at: #'onDowngrade' put: aBlock
]

{ #category : 'options api' }
MetacelloScriptExecutor >> onLock: aBlock [
  self options at: #'onLock' put: aBlock
]

{ #category : 'options api' }
MetacelloScriptExecutor >> onUpgrade: aBlock [
    self options at: #'onUpgrade' put: aBlock
]

{ #category : 'options api' }
MetacelloScriptExecutor >> onWarning: aBlock [
  self options at: #'onWarning' put: aBlock
]

{ #category : 'accessing' }
MetacelloScriptExecutor >> options [
    options ifNil: [ options := Dictionary new ].
    ^ options
]

{ #category : 'copying' }
MetacelloScriptExecutor >> postCopy [

	super postCopy.
	options := options copy
]

{ #category : 'actions api' }
MetacelloScriptExecutor >> record: required [
    actionArg := #'record:' -> {required}
]

{ #category : 'actions api' }
MetacelloScriptExecutor >> register [
  actionArg := #'register' -> #()
]

{ #category : 'args' }
MetacelloScriptExecutor >> repositoryArg [
    ^ repositoryArg
]

{ #category : 'args' }
MetacelloScriptExecutor >> repositoryArg: anObject [
    repositoryArg := anObject
]

{ #category : 'actions api' }
MetacelloScriptExecutor >> resolve: required [

	actionArg := #resolve: -> { required }
]

{ #category : 'options api' }
MetacelloScriptExecutor >> resolver: aMetacelloResolver [

	self options at: #resolver put: aMetacelloResolver
]

{ #category : 'accessing' }
MetacelloScriptExecutor >> results [
	"What the engine of each baseline the script names answered, in order"

	^ results ifNil: [ results := OrderedCollection new ]
]

{ #category : 'execution callback' }
MetacelloScriptExecutor >> shouldBeExecutedFor: aSpec [

	^ true
]

{ #category : 'accessing' }
MetacelloScriptExecutor >> singleResult [
	"Whether the script names one baseline, so it answers that baseline's result rather than a collection"

	^ singleResult ifNil: [ singleResult := false ]
]

{ #category : 'accessing' }
MetacelloScriptExecutor >> singleResult: aBoolean [

	singleResult := aBoolean
]

{ #category : 'actions api' }
MetacelloScriptExecutor >> unlock [
    actionArg := #'unlock' -> #()
]

{ #category : 'actions api' }
MetacelloScriptExecutor >> unregister [
  actionArg := #'unregister' -> #()
]
