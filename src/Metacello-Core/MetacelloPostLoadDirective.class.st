Class {
	#name : 'MetacelloPostLoadDirective',
	#superclass : 'MetacelloDirective',
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

	spec postLoadDoIt ifNotNil: [ aLoaderDirective add: self ]
]

{ #category : 'printing' }
MetacelloPostLoadDirective >> label [

	^ super label , ' >> ' , self spec postLoadDoIt value asString
]

{ #category : 'accessing' }
MetacelloPostLoadDirective >> title [

	^ 'postload'
]
