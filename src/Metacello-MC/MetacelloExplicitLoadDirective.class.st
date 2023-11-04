Class {
	#name : 'MetacelloExplicitLoadDirective',
	#superclass : 'MetacelloVersionLoadDirective',
	#category : 'Metacello-MC-Directives',
	#package : 'Metacello-MC',
	#tag : 'Directives'
}

{ #category : 'actions' }
MetacelloExplicitLoadDirective >> loadUsing: aLoaderDirective gofer: aGofer [

	"load has already been performed, no need to load again"
]

{ #category : 'actions' }
MetacelloExplicitLoadDirective >> loadWithPolicy: aLoadPolicy [

	| gofer |
	gofer := MetacelloGofer new.
	gofer disablePackageCache.
	gofer repository: aLoadPolicy cacheRepository.
	self loadLinearLoadDirective: self gofer: gofer
]

{ #category : 'accessing' }
MetacelloExplicitLoadDirective >> title [

	^'force load'
]
