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
MetacelloAtomicLoadingStrategy >> atomic [

	^ self
]

{ #category : 'actions' }
MetacelloAtomicLoadingStrategy >> executePackageDirective: aPackageLoadDirective gofer: aGofer [
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
