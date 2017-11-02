"
MetacelloScriptExecutor decodes script args and launches MetacelloScriptEngine to run for each projectSpec encounters...i,e., primarily here to handl array and block args to configuration:, baseline: or project:.
"
Class {
	#name : #MetacelloScriptExecutor,
	#superclass : #Object,
	#instVars : [
		'options',
		'roots',
		'singleRoot',
		'projectSpecGenerator',
		'actionArg',
		'configurationArg',
		'baselineArg',
		'projectArg',
		'classNameArg',
		'versionArg',
		'repositoryArg'
	],
	#category : 'Metacello-Core-Scripts'
}

{ #category : #execution }
MetacelloScriptExecutor >> applyArgsToProjectSpec: aProjectSpec [
    classNameArg ifNotNil: [ aProjectSpec className: classNameArg ].
    versionArg ifNotNil: [ aProjectSpec versionString: versionArg ].
    repositoryArg ifNotNil: [ aProjectSpec repository: repositoryArg ].
    ^ aProjectSpec
]

{ #category : #args }
MetacelloScriptExecutor >> baselineArg [
	^ baselineArg
]

{ #category : #args }
MetacelloScriptExecutor >> baselineArg: anObject [
	baselineArg := anObject
]

{ #category : #'options api' }
MetacelloScriptExecutor >> cacheRepository: aRepositoryDescription [
    self options at: #'cacheRepository' put: aRepositoryDescription
]

{ #category : #args }
MetacelloScriptExecutor >> classNameArg [
    ^ classNameArg
]

{ #category : #args }
MetacelloScriptExecutor >> classNameArg: anObject [
    classNameArg := anObject
]

{ #category : #args }
MetacelloScriptExecutor >> configurationArg [
    ^ configurationArg
]

{ #category : #args }
MetacelloScriptExecutor >> configurationArg: anObject [
    configurationArg := anObject
]

{ #category : #execution }
MetacelloScriptExecutor >> execute: statements [
  statements
    do: [ :assoc | 
      assoc value
        ifNil: [ self perform: assoc key ]
        ifNotNil: [ self perform: assoc key withArguments: assoc value ] ].
  projectSpecGenerator := self projectSpecGenerator.
  projectSpecGenerator target
    execute: [ :projectSpec | 
      | engine |
      engine := MetacelloScriptEngine new
        options: self options copy;
        projectSpec: projectSpec;
        yourself.
      engine perform: actionArg key withArguments: actionArg value.
      engine root ifNotNil: [ :root | self roots add: root ] ]
    against: self.
  ^ (self singleRoot and: [ self roots size == 1 ])
    ifTrue: [ self roots first ]
    ifFalse: [ self roots ]
]

{ #category : #'execution callback' }
MetacelloScriptExecutor >> executeBlock: selectBlock do: projectSpecBlock [
    ((projectSpecGenerator projectSpecListBlock value select: selectBlock) select: self projectSpecSelectBlock)
        do: [ :projectSpec | projectSpecBlock value: (self applyArgsToProjectSpec: projectSpec copy) ]
]

{ #category : #'execution callback' }
MetacelloScriptExecutor >> executeCollection: aCollection do: projectSpecBlock [
    aCollection
        do: [ :projectName | 
            ((projectSpecGenerator projectSpecLookupBlock value: projectName) select: self projectSpecSelectBlock)
                do: [ :projectSpec | projectSpecBlock value: (self applyArgsToProjectSpec: projectSpec copy) ] ]
]

{ #category : #'execution callback' }
MetacelloScriptExecutor >> executeString: aString do: projectSpecBlock [
  singleRoot ifNil: [ self singleRoot: true ].
  ((projectSpecGenerator projectSpecLookupBlock value: aString)
    select: self projectSpecSelectBlock)
    do: [ :projectSpec | projectSpecBlock value: (self applyArgsToProjectSpec: projectSpec copy) ]
]

{ #category : #'actions api' }
MetacelloScriptExecutor >> fetch: required [
    actionArg := #'fetch:' -> {required}
]

{ #category : #'actions api' }
MetacelloScriptExecutor >> get [
    actionArg := #'get' -> #()
]

{ #category : #'options api' }
MetacelloScriptExecutor >> ignoreImage: aBool [
    self options at: #'ignoreImage' put: aBool
]

{ #category : #'actions api' }
MetacelloScriptExecutor >> list [
  actionArg := #'list' -> #().
  self singleRoot: false
]

{ #category : #'actions api' }
MetacelloScriptExecutor >> load: required [
    actionArg := #'load:' -> {required}
]

{ #category : #'actions api' }
MetacelloScriptExecutor >> lock [
    actionArg := #'lock' -> #()
]

{ #category : #'options api' }
MetacelloScriptExecutor >> onConflict: aBlock [
    self options at: #'onConflict' put: aBlock
]

{ #category : #'options api' }
MetacelloScriptExecutor >> onDowngrade: aBlock [
    self options at: #'onDowngrade' put: aBlock
]

{ #category : #'options api' }
MetacelloScriptExecutor >> onLock: aBlock [
  self options at: #'onLock' put: aBlock
]

{ #category : #'options api' }
MetacelloScriptExecutor >> onUpgrade: aBlock [
    self options at: #'onUpgrade' put: aBlock
]

{ #category : #'options api' }
MetacelloScriptExecutor >> onWarning: aBlock [
  self options at: #'onWarning' put: aBlock
]

{ #category : #accessing }
MetacelloScriptExecutor >> options [
    options ifNil: [ options := Dictionary new ].
    ^ options
]

{ #category : #args }
MetacelloScriptExecutor >> projectArg [
    ^ projectArg
]

{ #category : #args }
MetacelloScriptExecutor >> projectArg: anObject [
    projectArg := anObject
]

{ #category : #execution }
MetacelloScriptExecutor >> projectSpecGenerator [
    baselineArg
        ifNotNil: [ 
            configurationArg ifNotNil: [ self error: ' baseline: and configuration: are both be specified' ].
            projectArg ifNotNil: [ self error: ' baseline: and project are both be specified' ].
            ^ MetacelloBaselineSpecGenerator new
                target: baselineArg;
                yourself ].
    configurationArg
        ifNotNil: [ 
            baselineArg ifNotNil: [ self error: ' baseline: and configuration: are both be specified' ].
            projectArg ifNotNil: [ self error: ' configuration and project are both be specified' ].
            ^ MetacelloConfigurationSpecGenerator new
                target: configurationArg;
                yourself ].
    projectArg
        ifNotNil: [ 
            configurationArg ifNotNil: [ self error: ' project and configuration: are both be specified' ].
            baselineArg ifNotNil: [ self error: ' baseline: and project are both be specified' ].
            ^ MetacelloProjectSpecGenerator new
                target: projectArg;
                yourself ].
    self error: 'project, baseline, or configuration not specified'
]

{ #category : #'execution callback' }
MetacelloScriptExecutor >> projectSpecSelectBlock [
    ^ [ :projectSpec | true ]
]

{ #category : #'actions api' }
MetacelloScriptExecutor >> record: required [
    actionArg := #'record:' -> {required}
]

{ #category : #'actions api' }
MetacelloScriptExecutor >> register [
  actionArg := #'register' -> #()
]

{ #category : #args }
MetacelloScriptExecutor >> repositoryArg [
    ^ repositoryArg
]

{ #category : #args }
MetacelloScriptExecutor >> repositoryArg: anObject [
    repositoryArg := anObject
]

{ #category : #'options api' }
MetacelloScriptExecutor >> repositoryOverrides: aRepositoryDescriptionList [
    self options at: #'repositoryOverrides' put: aRepositoryDescriptionList
]

{ #category : #accessing }
MetacelloScriptExecutor >> roots [
    roots ifNil: [ roots := OrderedCollection new ].
    ^ roots
]

{ #category : #'options api' }
MetacelloScriptExecutor >> silently: aBool [
    self options at: #'silently' put: aBool
]

{ #category : #accessing }
MetacelloScriptExecutor >> singleRoot [
  singleRoot ifNil: [ singleRoot := false ].
  ^ singleRoot
]

{ #category : #accessing }
MetacelloScriptExecutor >> singleRoot: aBool [
  singleRoot := aBool
]

{ #category : #'actions api' }
MetacelloScriptExecutor >> unlock [
    actionArg := #'unlock' -> #()
]

{ #category : #'actions api' }
MetacelloScriptExecutor >> unregister [
  actionArg := #'unregister' -> #()
]

{ #category : #'options api' }
MetacelloScriptExecutor >> useCurrentVersion: aBool [
    "private option used to implement the classic mode"

    self options at: #'useCurrentVersion' put: aBool
]

{ #category : #args }
MetacelloScriptExecutor >> versionArg [
    ^ versionArg
]

{ #category : #args }
MetacelloScriptExecutor >> versionArg: anObject [
    versionArg := anObject
]
