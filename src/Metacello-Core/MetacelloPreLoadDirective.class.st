Class {
	#name : 'MetacelloPreLoadDirective',
	#superclass : 'MetacelloPrePostLoadDirective',
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'visiting' }
MetacelloPreLoadDirective >> acceptVisitor: aTarget [
	
	aTarget visitPreLoadDirective: self
]

{ #category : 'actions' }
MetacelloPreLoadDirective >> addTo: aLoaderDirective [

	spec preLoadDoIt value ifNotNil: [ aLoaderDirective add: self ]
]

{ #category : 'actions' }
MetacelloPreLoadDirective >> executeUsing: aLoader [

	aLoader executePreloadDirective: self
]

{ #category : 'printing' }
MetacelloPreLoadDirective >> label [

	^super label, ' >> ', self spec preLoadDoIt value asString
]

{ #category : 'accessing' }
MetacelloPreLoadDirective >> title [

	^'preload'
]
