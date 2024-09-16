Class {
	#name : 'MetacelloLinearLoadDirective',
	#superclass : 'MetacelloVersionLoadDirective',
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'visiting' }
MetacelloLinearLoadDirective >> acceptVisitor: aVisitor [
	
	^ aVisitor visitLinearLoadDirective: self
]

{ #category : 'testing' }
MetacelloLinearLoadDirective >> includes: aDirective [ 
	
	^ self loadDirectives includes: aDirective
]

{ #category : 'accessing' }
MetacelloLinearLoadDirective >> title [

	^'linear load'
]
