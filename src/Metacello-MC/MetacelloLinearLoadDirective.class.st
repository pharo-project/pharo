Class {
	#name : #MetacelloLinearLoadDirective,
	#superclass : #MetacelloVersionLoadDirective,
	#category : #'Metacello-MC-Directives'
}

{ #category : #actions }
MetacelloLinearLoadDirective >> loadUsing: aLoaderDirective gofer: aGofer [

	self loadDirectives isEmpty ifTrue: [ ^self ].
	aLoaderDirective loadLinearLoadDirective: self gofer: aGofer.
]

{ #category : #accessing }
MetacelloLinearLoadDirective >> title [

	^'linear load'
]
