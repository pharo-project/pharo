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
		'loadsArg',
		'roots',
		'resolution'
	],
	#category : 'Metacello-Core-Scripts',
	#package : 'Metacello-Core',
	#tag : 'Scripts'
}

{ #category : 'args' }
MetacelloScriptExecutor >> addRoot: aProjectSpec [
	"Add the root aProjectSpec names, after the roots added before"

	self roots add: aProjectSpec
]

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
	"Perform the action with one engine on every root the script names"

	| engine |
	engine := MetacelloScriptEngine new
		          options: self options copy;
		          roots: self rootSpecs;
		          yourself.
	^ [
		  self class codeSupportAnnouncer announce: (MetacelloExecutionStartAnnouncement executor: self).
		  engine perform: actionArg key withArguments: actionArg value ] ensure: [
		  resolution := engine resolution.
		  self class codeSupportAnnouncer announce: (MetacelloExecutionEndAnnouncement executor: self) ]
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
	options := options copy.
	roots := roots copy
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

{ #category : 'accessing' }
MetacelloScriptExecutor >> resolution [
	"The MetacelloResolution of the last execution that resolved (load, record, resolve...), errors included; nil when none did"

	^ resolution
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

{ #category : 'execution callback' }
MetacelloScriptExecutor >> rootFor: aProjectSpec [
	"The project spec the action works on for the root aProjectSpec: a copy of its registered spec. Signal an error when it is not registered, or the script cannot run on it (see checkCanRunOn:)"

	| registered |
	registered := MetacelloProjectRegistration
		              projectSpecForClassNamed: aProjectSpec name
		              ifAbsent: [ NotFound signal: 'Project ' , aProjectSpec name , ' is not registered' ].
	self checkCanRunOn: registered.
	^ registered copy
]

{ #category : 'execution' }
MetacelloScriptExecutor >> rootSpecs [
	"The project specs of the roots the script names, in order: the one its baseline:, repository: and load: pieces build, then each root added (see addRoot:)"

	| all |
	all := OrderedCollection new.
	baselineArg ifNotNil: [ all add: (self applyArgsToProjectSpec: (self rootFor: (self specNamed: baselineArg))) ].
	self roots do: [ :each | all add: (self rootFor: each) ].
	all ifEmpty: [ MetacelloNoRootError signal: 'The script names no baseline' ].
	^ all asArray
]

{ #category : 'args' }
MetacelloScriptExecutor >> roots [
	"The roots added to the script, in order, besides the one its baseline:, repository: and load: pieces build"

	^ roots ifNil: [ roots := OrderedCollection new ]
]

{ #category : 'execution' }
MetacelloScriptExecutor >> specNamed: aString [
	"A new project spec for the baseline aString"

	^ (MetacelloProjectSpec project: MetacelloProject new)
		  name: aString;
		  className: 'BaselineOf' , aString;
		  yourself
]

{ #category : 'actions api' }
MetacelloScriptExecutor >> unlock [
    actionArg := #'unlock' -> #()
]

{ #category : 'actions api' }
MetacelloScriptExecutor >> unregister [
  actionArg := #'unregister' -> #()
]
