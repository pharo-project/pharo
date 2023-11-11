Class {
	#name : 'MetacelloPreLoadDirective',
	#superclass : 'MetacelloPrePostLoadDirective',
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'actions' }
MetacelloPreLoadDirective >> addTo: aLoaderDirective [

	spec preLoadDoIt value ifNotNil: [ aLoaderDirective add: self ]
]

{ #category : 'printing' }
MetacelloPreLoadDirective >> label [

	^super label, ' >> ', self spec preLoadDoIt value asString
]

{ #category : 'actions' }
MetacelloPreLoadDirective >> loadUsing: aLoaderDirective gofer: aGofer [

	aLoaderDirective loadPreloadDirective: self.
]

{ #category : 'accessing' }
MetacelloPreLoadDirective >> title [

	^'preload'
]
