Class {
	#name : 'MetacelloAtomicLoadingStrategy',
	#superclass : 'MetacelloLoadingStrategy',
	#instVars : [
		'packageloads',
		'preloads',
		'postloads'
	],
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'switch-strategy' }
MetacelloAtomicLoadingStrategy >> changeToAtomic [
	"If I'm atomic, just do nothing and continue as is"

	^ self
]

{ #category : 'switch-strategy' }
MetacelloAtomicLoadingStrategy >> changeToLinear [
	"If I'm atomic, close myself before starting a linear loading"

	self finalizeLoad.
	^ MetacelloLinearLoadingStrategy new
		  gofer: gofer;
		  yourself
]

{ #category : 'actions' }
MetacelloAtomicLoadingStrategy >> executePackageDirective: aPackageLoadDirective [
	"accumulate packages"

	packageloads ifNil: [ packageloads := OrderedCollection new ].
	packageloads add: aPackageLoadDirective
]

{ #category : 'loading' }
MetacelloAtomicLoadingStrategy >> executePostloadDirective: aPostloadDirective [
	"accumulate postloads"

	postloads add: aPostloadDirective
]

{ #category : 'loading' }
MetacelloAtomicLoadingStrategy >> executePreloadDirective: aPreloadDirective [
	"accumulate preloads"

	preloads add: aPreloadDirective
]

{ #category : 'actions' }
MetacelloAtomicLoadingStrategy >> finalizeLoad [
	"load the accumulated packages (if any), reset the package list"

	| pkgLoads |
	preloads do: [ :directive |
		self linear executePreloadDirective: directive ].
	preloads := nil.
	(pkgLoads := packageloads) notEmpty ifTrue: [
		MetacelloLoadTarget new
			loadPackageDirectives: pkgLoads
			gofer: self gofer.
		packageloads := nil ].
	postloads do: [ :directive |
		self linear executePostloadDirective: directive ].
	postloads := nil
]

{ #category : 'initialization' }
MetacelloAtomicLoadingStrategy >> initialize [

	super initialize.
	preloads := OrderedCollection new.
	postloads := OrderedCollection new.
	packageloads := OrderedCollection new
]

{ #category : 'switch-strategy' }
MetacelloAtomicLoadingStrategy >> linear [
	"Just return a linear strategy for myself, do not enforce anything.
	This is for internal use. Check users"
	^ MetacelloLinearLoadingStrategy new
		  gofer: gofer;
		  yourself
]
