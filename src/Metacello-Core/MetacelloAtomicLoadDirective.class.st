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
MetacelloAtomicLoadDirective >> executePackageDirective: aPackageLoadDirective gofer: aGofer [
	"accumulate packages"
	
	self packageloads add: aPackageLoadDirective
]

{ #category : 'loading' }
MetacelloAtomicLoadDirective >> executePostloadDirective: aPostloadDirective [
	"accumulate postloads"

	self postloads add: aPostloadDirective
]

{ #category : 'loading' }
MetacelloAtomicLoadDirective >> executePreloadDirective: aPreloadDirective [ 
	"accumulate preloads"

	self preloads add: aPreloadDirective
]

{ #category : 'actions' }
MetacelloAtomicLoadDirective >> executeUsing: aLoaderDirective gofer: aGofer [

	self loadDirectives isEmpty ifTrue: [ ^ self ].

	self loadDirectives do: [ :directive |
		directive executeUsing: self gofer: aGofer ].

	(aLoaderDirective isKindOf: MetacelloAtomicLoadDirective) ifFalse: [
		self finalizeLoad: aGofer ]
]

{ #category : 'actions' }
MetacelloAtomicLoadDirective >> finalizeLoad: aGofer [
	"load the accumulated packages (if any), reset the package list"

	| pkgLoads |
	self preloads do: [:directive | super executePreloadDirective: directive ].
	preloads := nil.
	(pkgLoads := self packageloads) notEmpty 
		ifTrue: [
			self loader loadingSpecLoader 
				loadPackageDirectives: pkgLoads 
				gofer: aGofer.
			self packageloads: nil ].
	self postloads do: [:directive | super executePostloadDirective: directive ].
	postloads := nil
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
