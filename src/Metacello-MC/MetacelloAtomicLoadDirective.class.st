Class {
	#name : #MetacelloAtomicLoadDirective,
	#superclass : #MetacelloVersionLoadDirective,
	#instVars : [
		'packageloads',
		'preloads',
		'postloads'
	],
	#category : 'Metacello-MC-Directives'
}

{ #category : #enumerating }
MetacelloAtomicLoadDirective >> directivesDo: aBlock [

	aBlock value: self.
	self preloads do: [:directive | directive directivesDo: aBlock ].
	self loadDirectives do: [:directive | directive directivesDo: aBlock ].
	self postloads do: [:directive | directive directivesDo: aBlock ].
]

{ #category : #actions }
MetacelloAtomicLoadDirective >> finalizeLoad: aGofer [
	"load the accumulated packages (if any), reset the package list"

	| pkgLoads |
	self preloads do: [:directive | super loadPreloadDirective: directive ].
	preloads := nil.
	(pkgLoads := self packageloads) notEmpty 
		ifTrue: [
			self loader loadingSpecLoader 
				loadPackageDirectives: pkgLoads 
				gofer: aGofer.
			self packageloads: nil ].
	self postloads do: [:directive | super loadPostloadDirective: directive ].
	postloads := nil
]

{ #category : #actions }
MetacelloAtomicLoadDirective >> loadAtomicLoadDirective: aLoaderDirective gofer: aGofer [

	aLoaderDirective loadDirectives do: [:directive | directive loadUsing: self gofer: aGofer ].
]

{ #category : #actions }
MetacelloAtomicLoadDirective >> loadLinearLoadDirective: aLoaderDirective gofer: aGofer [

	self finalizeLoad: aGofer.
	super loadLinearLoadDirective: aLoaderDirective gofer: aGofer
]

{ #category : #loading }
MetacelloAtomicLoadDirective >> loadPackageDirective: aPackageLoadDirective gofer: aGofer [
	"accumulate packages"
	
	self packageloads add: aPackageLoadDirective
]

{ #category : #loading }
MetacelloAtomicLoadDirective >> loadPostloadDirective: aPostloadDirective [
	"accumulate postloads"

	self postloads add: aPostloadDirective
]

{ #category : #loading }
MetacelloAtomicLoadDirective >> loadPreloadDirective: aPreloadDirective [ 
	"accumulate preloads"

	self preloads add: aPreloadDirective
]

{ #category : #actions }
MetacelloAtomicLoadDirective >> loadUsing: aLoaderDirective gofer: aGofer [

	self loadDirectives isEmpty ifTrue: [ ^self ].
	aLoaderDirective loadAtomicLoadDirective: self gofer: aGofer.
]

{ #category : #accessing }
MetacelloAtomicLoadDirective >> packageloads [

	packageloads == nil ifTrue: [ packageloads := OrderedCollection new ].
	^ packageloads
]

{ #category : #accessing }
MetacelloAtomicLoadDirective >> packageloads: anObject [
	packageloads := anObject
]

{ #category : #accessing }
MetacelloAtomicLoadDirective >> postloads [

	postloads == nil ifTrue: [ postloads := OrderedCollection new ].
	^ postloads
]

{ #category : #accessing }
MetacelloAtomicLoadDirective >> preloads [

	preloads == nil ifTrue: [ preloads := OrderedCollection new ].
	^ preloads
]

{ #category : #enumerating }
MetacelloAtomicLoadDirective >> prepostLoadDirectivesDo: aBlock [

	self preloads do: [:directive | directive prepostLoadDirectivesDo: aBlock ].
	self loadDirectives do: [:directive | directive prepostLoadDirectivesDo: aBlock ].
	self postloads do: [:directive | directive prepostLoadDirectivesDo: aBlock ].
]

{ #category : #enumerating }
MetacelloAtomicLoadDirective >> prepostLoadsDo: aBlock [

	self preloads do: [:directive | directive prepostLoadDo: aBlock ].
	self loadDirectives do: [:directive | directive prepostLoadDo: aBlock ].
	self postloads do: [:directive | directive prepostLoadDo: aBlock ].
]

{ #category : #accessing }
MetacelloAtomicLoadDirective >> title [

	^'atomic load'
]
