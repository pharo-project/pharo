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

{ #category : 'actions' }
MetacelloVersionLoadDirective >> executeUsing: oldLoadingStrategy [

	| newLoadingStrategy |
	self loadDirectives isEmpty ifTrue: [ ^ self ].

	newLoadingStrategy := self newLoadingStrategyFrom: oldLoadingStrategy.

	self loadDirectives do: [ :directive |
		directive executeUsing: newLoadingStrategy ].
	newLoadingStrategy finalizeLoadWith: self loader
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

{ #category : 'actions' }
MetacelloVersionLoadDirective >> loadWithPolicy: aLoadPolicy [

	self executeUsing: (self newWithEngine: aLoadPolicy)
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

	(spec isNil and: [ self loader isNotNil ]) ifTrue: [
		^ [ self loader spec versionSpec ]
			  on: MessageNotUnderstood
			  do: [ :ex | ex return: self loader spec ] ].
	^ spec
]
