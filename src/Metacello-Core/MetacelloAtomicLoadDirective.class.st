Class {
	#name : 'MetacelloAtomicLoadDirective',
	#superclass : 'MetacelloVersionLoadDirective',
	#instVars : [
		'packageloads',
		'preloads',
		'postloads'
	],
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'actions' }
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

{ #category : 'actions' }
MetacelloAtomicLoadDirective >> loadAtomicLoadDirective: aLoaderDirective gofer: aGofer [

	aLoaderDirective loadDirectives do: [:directive | directive loadUsing: self gofer: aGofer ].
]

{ #category : 'actions' }
MetacelloAtomicLoadDirective >> loadLinearLoadDirective: aLoaderDirective gofer: aGofer [

	self finalizeLoad: aGofer.
	super loadLinearLoadDirective: aLoaderDirective gofer: aGofer
]

{ #category : 'actions' }
MetacelloAtomicLoadDirective >> loadPackageDirective: aPackageLoadDirective gofer: aGofer [
	"accumulate packages"
	
	self packageloads add: aPackageLoadDirective
]

{ #category : 'loading' }
MetacelloAtomicLoadDirective >> loadPostloadDirective: aPostloadDirective [
	"accumulate postloads"

	self postloads add: aPostloadDirective
]

{ #category : 'loading' }
MetacelloAtomicLoadDirective >> loadPreloadDirective: aPreloadDirective [ 
	"accumulate preloads"

	self preloads add: aPreloadDirective
]

{ #category : 'actions' }
MetacelloAtomicLoadDirective >> loadUsing: aLoaderDirective gofer: aGofer [

	self loadDirectives isEmpty ifTrue: [ ^self ].
	aLoaderDirective loadAtomicLoadDirective: self gofer: aGofer.
]

{ #category : 'accessing' }
MetacelloAtomicLoadDirective >> packageloads [

	^ packageloads ifNil: [ packageloads := OrderedCollection new ]
]

{ #category : 'accessing' }
MetacelloAtomicLoadDirective >> packageloads: anObject [
	packageloads := anObject
]

{ #category : 'accessing' }
MetacelloAtomicLoadDirective >> postloads [

	^ postloads ifNil: [ postloads := OrderedCollection new ]
]

{ #category : 'accessing' }
MetacelloAtomicLoadDirective >> preloads [

	^ preloads ifNil: [ preloads := OrderedCollection new ]
]

{ #category : 'accessing' }
MetacelloAtomicLoadDirective >> title [

	^'atomic load'
]
