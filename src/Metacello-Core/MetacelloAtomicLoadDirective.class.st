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

{ #category : 'as yet unclassified' }
MetacelloAtomicLoadDirective >> executeUsing: aLoadingStrategy [

	| loadingStrategy |
	self loadDirectives isEmpty ifTrue: [ ^ self ].

	loadingStrategy := aLoadingStrategy atomic.
	self loadDirectives do: [ :directive |
		directive executeUsing: loadingStrategy ].

	(loadingStrategy isKindOf: MetacelloAtomicLoadingStrategy) ifTrue: [
		loadingStrategy finalizeLoadWith: self loader ]
]

{ #category : 'actions' }
MetacelloAtomicLoadDirective >> newLoaderForPolicy: aLoadPolicy [

	| gofer |
	gofer := MetacelloGofer new.
	gofer disablePackageCache.
	gofer repository: aLoadPolicy cacheRepository.
	^ MetacelloAtomicLoadingStrategy new
		gofer: gofer;
		loader: self loader;
		yourself
]

{ #category : 'accessing' }
MetacelloAtomicLoadDirective >> title [

	^'atomic load'
]
