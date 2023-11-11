Class {
	#name : 'MetacelloPreLoadDirective',
	#superclass : 'MetacelloPrePostLoadDirective',
	#category : 'Metacello-MC-Directives',
	#package : 'Metacello-MC',
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

{ #category : 'actions' }
MetacelloPreLoadDirective >> preLoadDo: aBlock [

	aBlock value: self
]

{ #category : 'accessing' }
MetacelloPreLoadDirective >> title [

	^'preload'
]
