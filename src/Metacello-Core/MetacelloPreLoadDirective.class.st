Class {
	#name : 'MetacelloPreLoadDirective',
	#superclass : 'MetacelloDirective',
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

	spec preLoadDoIt ifNotNil: [ aLoaderDirective add: self ]
]

{ #category : 'printing' }
MetacelloPreLoadDirective >> label [

	^super label, ' >> ', self spec preLoadDoIt value asString
]

{ #category : 'accessing' }
MetacelloPreLoadDirective >> title [

	^'preload'
]
