Class {
	#name : 'MetacelloPostLoadDirective',
	#superclass : 'MetacelloPrePostLoadDirective',
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'actions' }
MetacelloPostLoadDirective >> addTo: aLoaderDirective [

	spec postLoadDoIt value ifNotNil: [ aLoaderDirective add: self ]
]

{ #category : 'printing' }
MetacelloPostLoadDirective >> label [

	^super label, ' >> ', self spec postLoadDoIt value asString
]

{ #category : 'actions' }
MetacelloPostLoadDirective >> loadUsing: aLoaderDirective gofer: aGofer [

	aLoaderDirective loadPostloadDirective: self.
]

{ #category : 'accessing' }
MetacelloPostLoadDirective >> title [

	^'postload'
]
