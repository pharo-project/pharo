Class {
	#name : 'MetacelloLinearLoadDirective',
	#superclass : 'MetacelloVersionLoadDirective',
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'private' }
MetacelloLinearLoadDirective >> newLoadingStrategyFrom: oldLoadingStrategy [

	^ oldLoadingStrategy changeToLinear
]

{ #category : 'actions' }
MetacelloLinearLoadDirective >> newWithEngine: aLoadPolicy [

	| gofer |
	gofer := Gofer new.
	gofer disablePackageCache.
	gofer repository: aLoadPolicy cacheRepository.
	^ MetacelloLinearLoadingStrategy new
		  gofer: gofer;
		  yourself
]

{ #category : 'accessing' }
MetacelloLinearLoadDirective >> title [

	^'linear load'
]
