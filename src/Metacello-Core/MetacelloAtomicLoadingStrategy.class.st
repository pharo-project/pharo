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

{ #category : 'as yet unclassified' }
MetacelloAtomicLoadingStrategy >> changeToAtomicWithLoader: anObject [
	"If I'm atomic, just do nothing and continue as is"

	^ self
]

{ #category : 'as yet unclassified' }
MetacelloAtomicLoadingStrategy >> changeToLinearWithLoader: aLoader [
	"If I'm atomic, close myself before starting a linear loading"

	self finalizeLoadWith: aLoader.
	^ MetacelloLinearLoadingStrategy new
		  gofer: gofer;
		  loader: aLoader;
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
MetacelloAtomicLoadingStrategy >> finalizeLoadWith: realLoader [
	"load the accumulated packages (if any), reset the package list"

	| pkgLoads |
	preloads do: [ :directive |
		self linear executePreloadDirective: directive ].
	preloads := nil.
	(pkgLoads := packageloads) notEmpty ifTrue: [
		realLoader loadingSpecLoader
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
