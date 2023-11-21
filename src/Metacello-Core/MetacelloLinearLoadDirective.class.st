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
	aLoaderDirective executeLinearLoadDirective: self gofer: aGofer.
]

{ #category : 'accessing' }
MetacelloLinearLoadDirective >> title [

	^'linear load'
]
