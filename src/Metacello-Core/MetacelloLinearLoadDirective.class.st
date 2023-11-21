Class {
	#name : 'MetacelloLinearLoadDirective',
	#superclass : 'MetacelloVersionLoadDirective',
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'actions' }
MetacelloLinearLoadDirective >> executeUsing: aLoaderDirective gofer: aGofer [

	self loadDirectives isEmpty ifTrue: [ ^ self ].

	(aLoaderDirective isKindOf: MetacelloAtomicLoadDirective) ifTrue: [
		aLoaderDirective finalizeLoad: aGofer ].

	self loadDirectives do: [ :directive |
		directive executeUsing: self gofer: aGofer ]
]

{ #category : 'accessing' }
MetacelloLinearLoadDirective >> title [

	^'linear load'
]
