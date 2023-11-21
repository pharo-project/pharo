Class {
	#name : 'MetacelloLinearLoadDirective',
	#superclass : 'MetacelloVersionLoadDirective',
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'actions' }
MetacelloLinearLoadDirective >> executeUsing: aLoaderDirective gofer: aGofer [

	self loadDirectives isEmpty ifTrue: [ ^self ].
	aLoaderDirective loadLinearLoadDirective: self gofer: aGofer.
]

{ #category : 'actions' }
MetacelloLinearLoadDirective >> loadWithPolicy: aLoadPolicy [

	| gofer |
	gofer := MetacelloGofer new.
	gofer disablePackageCache.
	gofer repository: aLoadPolicy cacheRepository.
	self executeUsing: self gofer: gofer
]

{ #category : 'accessing' }
MetacelloLinearLoadDirective >> title [

	^'linear load'
]
