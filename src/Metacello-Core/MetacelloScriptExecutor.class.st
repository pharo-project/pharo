"
MetacelloScriptExecutor decodes script args and launches MetacelloScriptEngine to run for each projectSpec encounters...i,e., primarily here to handl array and block args to configuration:, baseline: or project:.
"
Class {
	#name : 'MetacelloScriptExecutor',
	#superclass : 'Object',
	#instVars : [
		'options',
		'roots',
		'singleRoot',
		'actionArg',
		'baselineArg',
		'classNameArg',
		'repositoryArg'
	],
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'baselines' }
MetacelloScriptExecutor class >> baselineNameFrom: baseName [
	"Return the fully-qualified configuration class name."

	^ (baseName indexOfSubCollection: 'BaselineOf') > 0
		  ifTrue: [ baseName ]
		  ifFalse: [ 'BaselineOf' , baseName ]
]

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
				          projectReferenceSpec: projectReferenceSpec;
				          yourself.
			[
				self class codeSupportAnnouncer announce: (MetacelloExecutionStartAnnouncement executor: self).
				engine perform: actionArg key withArguments: actionArg value ] ensure: [
				self class codeSupportAnnouncer announce: (MetacelloExecutionEndAnnouncement executor: self) ].
			engine root ifNotNil: [ :root | self roots add: root ] ].
	^ (self singleRoot and: [ self roots size == 1 ])
		  ifTrue: [ self roots first ]
		  ifFalse: [ self roots ]
]

{ #category : 'execution callback' }
MetacelloScriptExecutor >> executeString: aString do: projectSpecBlock [

	| projectSpec |
	singleRoot ifNil: [ self singleRoot: true ].
	projectSpec := MetacelloProjectRegistration projectSpecForClassNamed: (self class baselineNameFrom: aString) ifAbsent: [ ].

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
  self singleRoot: false
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

{ #category : 'accessing' }
MetacelloScriptExecutor >> roots [

	^ roots ifNil: [ roots := OrderedCollection new ]
]

{ #category : 'execution callback' }
MetacelloScriptExecutor >> shouldBeExecutedFor: aSpec [

	^ true
]

{ #category : 'accessing' }
MetacelloScriptExecutor >> singleRoot [

	^ singleRoot ifNil: [ singleRoot := false ]
]

{ #category : 'accessing' }
MetacelloScriptExecutor >> singleRoot: aBoolean [

	singleRoot := aBoolean
]

{ #category : 'actions api' }
MetacelloScriptExecutor >> unlock [
    actionArg := #'unlock' -> #()
]

{ #category : 'actions api' }
MetacelloScriptExecutor >> unregister [
  actionArg := #'unregister' -> #()
]
