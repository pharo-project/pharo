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

{ #category : 'testing' }
MetacelloVersionLoadDirective >> includes: aDirective [
	
	self subclassResponsibility
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

{ #category : 'enumerating' }
MetacelloVersionLoadDirective >> packageDirectivesDo: aBlock [

	self loadDirectives do: [:directive | directive packageDirectivesDo: aBlock ].
]

{ #category : 'accessing' }
MetacelloVersionLoadDirective >> packages [
	
	^ loadDirectives flatCollect: [ :e | e packages ]
]

{ #category : 'printing' }
MetacelloVersionLoadDirective >> printOn: aStream indent: indent [

	super printOn: aStream indent: indent.
	self loadDirectives do: [ :each |
		aStream cr.
		each printOn: aStream indent: indent + 1 ]
]
