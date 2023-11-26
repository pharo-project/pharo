Class {
	#name : 'MetacelloVersionLoadDirective',
	#superclass : 'MetacelloDirective',
	#instVars : [
		'loadDirectives'
	],
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'actions' }
MetacelloVersionLoadDirective >> add: aDirective [

	self loadDirectives add: aDirective
]

{ #category : 'printing' }
MetacelloVersionLoadDirective >> label [

	^ self spec
		  ifNil: [ '' ]
		  ifNotNil: [ :aSpec | aSpec label ]
]

{ #category : 'accessing' }
MetacelloVersionLoadDirective >> loadDirectives [

	^ loadDirectives ifNil: [ loadDirectives := OrderedCollection new ]
]

{ #category : 'accessing' }
MetacelloVersionLoadDirective >> loadDirectives: anObject [
	loadDirectives := anObject
]

{ #category : 'private' }
MetacelloVersionLoadDirective >> newLoadingStrategyFrom: oldLoadingStrategy [

	self subclassResponsibility
]

{ #category : 'actions' }
MetacelloVersionLoadDirective >> newWithEngine: aLoadPolicy [

	self subclassResponsibility
]

{ #category : 'enumerating' }
MetacelloVersionLoadDirective >> packageDirectivesDo: aBlock [

	self loadDirectives do: [:directive | directive packageDirectivesDo: aBlock ].
]

{ #category : 'printing' }
MetacelloVersionLoadDirective >> printLoadDirectivesOn: aStream indent: indent [

	self loadDirectives do: [:each |
		aStream cr. 
		each printOn: aStream indent: indent + 1 ].
]

{ #category : 'printing' }
MetacelloVersionLoadDirective >> printOn: aStream indent: indent [

	super printOn: aStream indent: indent.
	self printLoadDirectivesOn: aStream indent: indent
]

{ #category : 'accessing' }
MetacelloVersionLoadDirective >> spec [
	"Expected to be a MetacelloVersionSpec"

	^ spec
]
