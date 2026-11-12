"
MetacelloScriptExecutor decodes script args and launches MetacelloScriptEngine to run for each projectSpec encounters...i,e., primarily here to handl array and block args to configuration:, baseline: or project:.
"
Class {
	#name : 'MetacelloScriptExecutor',
	#superclass : 'Object',
	#instVars : [
		'options',
		'actionArg',
		'baselineArg',
		'classNameArg',
		'repositoryArg',
		'loadsArg'
	],
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'execution' }
MetacelloScriptExecutor >> applyArgsToProjectSpec: aProjectSpec [

	classNameArg ifNotNil: [ aProjectSpec className: classNameArg ].
	repositoryArg ifNotNil: [ aProjectSpec repository: repositoryArg ].
	loadsArg ifNotNil: [ aProjectSpec loads: loadsArg ].
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

{ #category : 'execution callback' }
MetacelloScriptExecutor >> checkCanRunOn: aProjectSpec [
	"Signal an error if the script cannot run on the registered aProjectSpec. Any registered project will do"

	
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
	^ self executeString: baselineArg do: [ :projectReferenceSpec |
			  | engine |
			  engine := MetacelloScriptEngine new
				            options: self options copy;
				            projectSpec: projectReferenceSpec;
				            yourself.
			  [
				  self class codeSupportAnnouncer announce: (MetacelloExecutionStartAnnouncement executor: self).
				  engine perform: actionArg key withArguments: actionArg value ] ensure: [
				  self class codeSupportAnnouncer announce: (MetacelloExecutionEndAnnouncement executor: self) ] ]
]

{ #category : 'execution callback' }
MetacelloScriptExecutor >> executeString: aString do: projectSpecBlock [

	| projectSpec |
	projectSpec := MetacelloProjectRegistration
		               projectSpecForClassNamed: aString
		               ifAbsent: [ NotFound signal: 'Project ' , aString , ' is not registered' ].
	self checkCanRunOn: projectSpec.
	^ projectSpecBlock value: (self applyArgsToProjectSpec: projectSpec copy)
]

{ #category : 'actions api' }
MetacelloScriptExecutor >> fetch: required [

	loadsArg := required.
	actionArg := #fetch -> #(  )
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

	actionArg := #list -> #(  )
]

{ #category : 'actions api' }
MetacelloScriptExecutor >> load: required [

	loadsArg := required.
	actionArg := #load -> #(  )
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

	loadsArg := required.
	actionArg := #record -> #(  )
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

	loadsArg := required.
	actionArg := #resolve -> #(  )
]

{ #category : 'options api' }
MetacelloScriptExecutor >> resolver: aMetacelloResolver [

	self options at: #resolver put: aMetacelloResolver
]

{ #category : 'actions api' }
MetacelloScriptExecutor >> unlock [
    actionArg := #'unlock' -> #()
]

{ #category : 'actions api' }
MetacelloScriptExecutor >> unregister [
  actionArg := #'unregister' -> #()
]
