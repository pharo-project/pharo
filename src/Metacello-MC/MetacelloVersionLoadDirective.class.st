Class {
	#name : #MetacelloVersionLoadDirective,
	#superclass : #MetacelloDirective,
	#instVars : [
		'loadDirectives'
	],
	#category : #'Metacello-MC-Directives'
}

{ #category : #actions }
MetacelloVersionLoadDirective >> add: aDirective [

	self loadDirectives add: aDirective
]

{ #category : #enumerating }
MetacelloVersionLoadDirective >> directivesDo: aBlock [

	aBlock value: self.
	self loadDirectives do: [:directive | directive directivesDo: aBlock ].
]

{ #category : #actions }
MetacelloVersionLoadDirective >> finalizeLoad: aGofer [
	"nothing special for linear loads"
]

{ #category : #testing }
MetacelloVersionLoadDirective >> isExplicit [

	^false
]

{ #category : #printing }
MetacelloVersionLoadDirective >> label [

	self spec == nil ifTrue: [ ^'' ].
	^self spec label
]

{ #category : #actions }
MetacelloVersionLoadDirective >> loadAtomicLoadDirective: aLoaderDirective gofer: aGofer [

	aLoaderDirective loadDirectives do: [:directive | directive loadUsing: aLoaderDirective gofer: aGofer ].
	aLoaderDirective finalizeLoad: aGofer.
]

{ #category : #accessing }
MetacelloVersionLoadDirective >> loadDirectives [

	loadDirectives == nil ifTrue: [ loadDirectives := OrderedCollection new ].
	^ loadDirectives
]

{ #category : #accessing }
MetacelloVersionLoadDirective >> loadDirectives: anObject [
	loadDirectives := anObject
]

{ #category : #actions }
MetacelloVersionLoadDirective >> loadExplicitLoadDirective: aLoaderDirective gofer: aGofer [
	"load has already been performed, no need to load again"
]

{ #category : #actions }
MetacelloVersionLoadDirective >> loadLinearLoadDirective: aLoaderDirective gofer: aGofer [

	aLoaderDirective loadDirectives do: [:directive | directive loadUsing: aLoaderDirective gofer: aGofer ].
	aLoaderDirective finalizeLoad: aGofer.
]

{ #category : #loading }
MetacelloVersionLoadDirective >> loadPackageDirective: aPackageLoadDirective gofer: aGofer [

	aPackageLoadDirective loader loadingSpecLoader 
		loadPackageDirective: aPackageLoadDirective 
		gofer: aGofer
]

{ #category : #loading }
MetacelloVersionLoadDirective >> loadPostloadDirective: aPostloadDirective [ 

	| block |
	(block :=  aPostloadDirective spec postLoadDoItBlock) ~~ nil
		ifTrue: [
			aPostloadDirective evaluateSupplyingAnswers: [ block valueWithPossibleArgs: 
					(Array 
						with: aPostloadDirective loader 
						with: aPostloadDirective spec) ].
			MetacelloNotification signal: ('Evaluated -> ', aPostloadDirective spec label, ' >> ', aPostloadDirective spec postLoadDoIt value asString)]
]

{ #category : #loading }
MetacelloVersionLoadDirective >> loadPreloadDirective: aPreloadDirective [ 

	| block |
	(block :=  aPreloadDirective spec preLoadDoItBlock) ~~ nil
		ifTrue: [
			aPreloadDirective evaluateSupplyingAnswers: [ block valueWithPossibleArgs: 
					(Array 
						with: aPreloadDirective loader 
						with: aPreloadDirective spec) ].
			MetacelloNotification signal: ('Evaluated -> ', aPreloadDirective spec label, ' >> ', aPreloadDirective spec preLoadDoIt value asString)]
]

{ #category : #actions }
MetacelloVersionLoadDirective >> loadWithPolicy: aLoadPolicy [

	| gofer |
	gofer := MetacelloGofer new.
	gofer disablePackageCache.
	gofer repository: aLoadPolicy cacheRepository.
	self loadUsing: self gofer: gofer
]

{ #category : #enumerating }
MetacelloVersionLoadDirective >> packageDirectivesDo: aBlock [

	self loadDirectives do: [:directive | directive packageDirectivesDo: aBlock ].
]

{ #category : #enumerating }
MetacelloVersionLoadDirective >> packagesDo: aBlock [

	self loadDirectives do: [:directive | directive packageDo: aBlock ].
]

{ #category : #enumerating }
MetacelloVersionLoadDirective >> prepostLoadDirectivesDo: aBlock [

	self loadDirectives do: [:directive | directive prepostLoadDirectivesDo: aBlock ].
]

{ #category : #enumerating }
MetacelloVersionLoadDirective >> prepostLoadsDo: aBlock [

	self loadDirectives do: [:directive | directive prepostLoadDo: aBlock ].
]

{ #category : #printing }
MetacelloVersionLoadDirective >> printLoadDirectivesOn: aStream indent: indent [

	self loadDirectives do: [:each |
		aStream cr. 
		each printOn: aStream indent: indent + 1 ].
]

{ #category : #printing }
MetacelloVersionLoadDirective >> printOn: aStream indent: indent [

	super printOn: aStream indent: indent.
	self printLoadDirectivesOn: aStream indent: indent
]

{ #category : #accessing }
MetacelloVersionLoadDirective >> spec [
	"Expected to be a MetacelloVersionSpec"
	
	(spec == nil  and: [  self loader ~~ nil ]) 
		ifTrue: [ 
			^[ self loader spec versionSpec ] 
				on: MessageNotUnderstood
				do: [:ex | ex return: self loader spec ]].
	^spec
]

{ #category : #enumerating }
MetacelloVersionLoadDirective >> versionDirectivesDepthFirstDo: aBlock [

	self loadDirectives do: [:directive | directive versionDirectivesDepthFirstDo: aBlock ].
	aBlock value: self.
]

{ #category : #enumerating }
MetacelloVersionLoadDirective >> versionDirectivesDo: aBlock [

	aBlock value: self.
	self loadDirectives do: [:directive | directive versionDirectivesDo: aBlock ].
]

{ #category : #enumerating }
MetacelloVersionLoadDirective >> versionDo: aBlock [

	aBlock value: self.
]

{ #category : #enumerating }
MetacelloVersionLoadDirective >> versionsDo: aBlock [

	self loadDirectives do: [:directive | directive versionDo: aBlock ].
]
