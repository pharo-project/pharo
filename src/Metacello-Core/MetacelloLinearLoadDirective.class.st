Class {
	#name : 'MetacelloLinearLoadDirective',
	#superclass : 'MetacelloVersionLoadDirective',
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

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

{ #category : 'as yet unclassified' }
MetacelloLinearLoadDirective >> newLoadingStrategyFrom: oldLoadingStrategy [

	^ oldLoadingStrategy changeToLinearWithLoader: self loader
]

{ #category : 'accessing' }
MetacelloLinearLoadDirective >> title [

	^'linear load'
]
