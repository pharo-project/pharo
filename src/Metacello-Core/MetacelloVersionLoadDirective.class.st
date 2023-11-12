Class {
	#name : 'MetacelloVersionLoadDirective',
	#superclass : 'MetacelloDirective',
	#instVars : [
		'loadDirectives'
	],
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'actions' }
MetacelloVersionLoadDirective >> add: aDirective [

	self loadDirectives add: aDirective
]

{ #category : 'actions' }
MetacelloVersionLoadDirective >> finalizeLoad: aGofer [
	"nothing special for linear loads"
]

{ #category : 'printing' }
MetacelloVersionLoadDirective >> label [

	^ self spec
		  ifNil: [ '' ]
		  ifNotNil: [ :aSpec | aSpec label ]
]

{ #category : 'actions' }
MetacelloVersionLoadDirective >> loadAtomicLoadDirective: aLoaderDirective gofer: aGofer [

	aLoaderDirective loadDirectives do: [:directive | directive loadUsing: aLoaderDirective gofer: aGofer ].
	aLoaderDirective finalizeLoad: aGofer.
]

{ #category : 'accessing' }
MetacelloVersionLoadDirective >> loadDirectives [

	^ loadDirectives ifNil: [ loadDirectives := OrderedCollection new ]
]

{ #category : 'accessing' }
MetacelloVersionLoadDirective >> loadDirectives: anObject [
	loadDirectives := anObject
]

{ #category : 'actions' }
MetacelloVersionLoadDirective >> loadLinearLoadDirective: aLoaderDirective gofer: aGofer [

	aLoaderDirective loadDirectives do: [:directive | directive loadUsing: aLoaderDirective gofer: aGofer ].
	aLoaderDirective finalizeLoad: aGofer.
]

{ #category : 'actions' }
MetacelloVersionLoadDirective >> loadPackageDirective: aPackageLoadDirective gofer: aGofer [

	aPackageLoadDirective loader loadingSpecLoader 
		loadPackageDirective: aPackageLoadDirective 
		gofer: aGofer
]

{ #category : 'loading' }
MetacelloVersionLoadDirective >> loadPostloadDirective: aPostloadDirective [

	aPostloadDirective spec postLoadDoItBlock ifNotNil: [ :block |
		MetacelloNotification signal: 'Evaluating -> ' , aPostloadDirective spec label , ' >> ' , aPostloadDirective spec postLoadDoIt value asString.
		aPostloadDirective evaluateSupplyingAnswers: [ block valueWithPossibleArgs: (Array with: aPostloadDirective loader with: aPostloadDirective spec) ].
		MetacelloNotification signal: 'Evaluated -> ' , aPostloadDirective spec label , ' >> ' , aPostloadDirective spec postLoadDoIt value asString ]
]

{ #category : 'loading' }
MetacelloVersionLoadDirective >> loadPreloadDirective: aPreloadDirective [

	aPreloadDirective spec preLoadDoItBlock ifNotNil: [ :block |
		MetacelloNotification signal: 'Evaluating -> ' , aPreloadDirective spec label , ' >> ' , aPreloadDirective spec preLoadDoIt value asString.
		aPreloadDirective evaluateSupplyingAnswers: [ block valueWithPossibleArgs: (Array with: aPreloadDirective loader with: aPreloadDirective spec) ].
		MetacelloNotification signal: 'Evaluated -> ' , aPreloadDirective spec label , ' >> ' , aPreloadDirective spec preLoadDoIt value asString ]
]

{ #category : 'actions' }
MetacelloVersionLoadDirective >> loadWithPolicy: aLoadPolicy [

	| gofer |
	gofer := MetacelloGofer new.
	gofer disablePackageCache.
	gofer repository: aLoadPolicy cacheRepository.
	self loadUsing: self gofer: gofer
]

{ #category : 'enumerating' }
MetacelloVersionLoadDirective >> packageDirectivesDo: aBlock [

	self loadDirectives do: [:directive | directive packageDirectivesDo: aBlock ].
]

{ #category : 'printing' }
MetacelloVersionLoadDirective >> printLoadDirectivesOn: aStream indent: indent [

	self loadDirectives do: [:each |
		aStream cr. 
		each printOn: aStream indent: indent + 1 ].
]

{ #category : 'printing' }
MetacelloVersionLoadDirective >> printOn: aStream indent: indent [

	super printOn: aStream indent: indent.
	self printLoadDirectivesOn: aStream indent: indent
]

{ #category : 'accessing' }
MetacelloVersionLoadDirective >> spec [
	"Expected to be a MetacelloVersionSpec"

	(spec isNil and: [ self loader isNotNil ]) ifTrue: [
		^ [ self loader spec versionSpec ]
			  on: MessageNotUnderstood
			  do: [ :ex | ex return: self loader spec ] ].
	^ spec
]
