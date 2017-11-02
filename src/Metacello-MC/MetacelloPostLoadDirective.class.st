Class {
	#name : #MetacelloPostLoadDirective,
	#superclass : #MetacelloPrePostLoadDirective,
	#category : 'Metacello-MC-Directives'
}

{ #category : #actions }
MetacelloPostLoadDirective >> addTo: aLoaderDirective [

	spec postLoadDoIt value ~~ nil ifTrue: [ aLoaderDirective add: self ]
]

{ #category : #printing }
MetacelloPostLoadDirective >> label [

	^super label, ' >> ', self spec postLoadDoIt value asString
]

{ #category : #actions }
MetacelloPostLoadDirective >> loadUsing: aLoaderDirective gofer: aGofer [

	aLoaderDirective loadPostloadDirective: self.
]

{ #category : #actions }
MetacelloPostLoadDirective >> postLoadDo: aBlock [

	aBlock value: self
]

{ #category : #accessing }
MetacelloPostLoadDirective >> title [

	^'postload'
]
