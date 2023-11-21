Class {
	#name : 'MetacelloLinearLoadDirective',
	#superclass : 'MetacelloVersionLoadDirective',
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'as yet unclassified' }
MetacelloLinearLoadDirective >> executeUsing: aLoadingStrategy [

	self loadDirectives isEmpty ifTrue: [ ^ self ].

	(aLoadingStrategy isKindOf: MetacelloAtomicLoadingStrategy) ifTrue: [
		aLoadingStrategy finalizeLoadWith: aLoadingStrategy loader ].

	self loadDirectives do: [ :directive |
		directive executeUsing: aLoadingStrategy linear ]
]

{ #category : 'actions' }
MetacelloLinearLoadDirective >> newLoaderForPolicy: aLoadPolicy [

	| gofer |
	gofer := MetacelloGofer new.
	gofer disablePackageCache.
	gofer repository: aLoadPolicy cacheRepository.
	^ MetacelloLinearLoadingStrategy new
		  gofer: gofer;
		  loader: self loader;
		  yourself
]

{ #category : 'accessing' }
MetacelloLinearLoadDirective >> title [

	^'linear load'
]
