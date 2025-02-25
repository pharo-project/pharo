Class {
	#name : 'MetacelloPostLoadDirective',
	#superclass : 'MetacelloPrePostLoadDirective',
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'visiting' }
MetacelloPostLoadDirective >> acceptVisitor: aTarget [
	
	aTarget visitPostLoadDirective: self
]

{ #category : 'actions' }
MetacelloPostLoadDirective >> addTo: aLoaderDirective [

	spec postLoadDoIt value ifNotNil: [ aLoaderDirective add: self ]
]

{ #category : 'printing' }
MetacelloPostLoadDirective >> label [

	^super label, ' >> ', self spec postLoadDoIt value asString
]

{ #category : 'accessing' }
MetacelloPostLoadDirective >> title [

	^'postload'
]
