"
I'm a model to be used when building groups for outline dataSources.
I keep groups with elements.
I can provide also a contexts for menu operations and other kind of options to be associated with group.
"
Class {
	#name : #IceTipTreeGroup,
	#superclass : #IceTipTreeNode,
	#instVars : [
		'name',
		'children'
	],
	#category : #'Iceberg-TipUI-View-DataSource'
}

{ #category : #'instance creation' }
IceTipTreeGroup class >> name: aName children: aCollection [
	^ self new 
		name: aName;
		children: aCollection;
		yourself
]

{ #category : #'instance creation' }
IceTipTreeGroup class >> name: aName icon: anIcon children: aCollection [
	^ self new 
		name: aName;
		icon: anIcon;
		children: aCollection;
		yourself
]

{ #category : #accessing }
IceTipTreeGroup >> addChild: anObject [
	children ifNil: [ children := OrderedCollection new ].
	children add: anObject.
	anObject parent: self.
	^ anObject
]

{ #category : #accessing }
IceTipTreeGroup >> children [
	^ children ifNil: [ #() ]
]

{ #category : #accessing }
IceTipTreeGroup >> children: aCollection [
	children := aCollection.
	children do: [ :each | each parent: self ]
]

{ #category : #accessing }
IceTipTreeGroup >> descriptionWithDecoration [
	^ self name asText
]

{ #category : #accessing }
IceTipTreeGroup >> ensurePath: aPath [
	| first next |
	
	first := aPath first.
	next := self children 
		detect: [ :each | each isSameAs: first ]
		ifNone: [ self addChild: (first copy children: OrderedCollection new) ].
	^ aPath allButFirst 
		ifNotEmpty: [ :remainingPath | next ensurePath: remainingPath ]
		ifEmpty: [ next ]
]

{ #category : #accessing }
IceTipTreeGroup >> icon [
	^ icon
]

{ #category : #testing }
IceTipTreeGroup >> isGroup [
	^ true
]

{ #category : #testing }
IceTipTreeGroup >> isSameAs: aNode [
	^ aNode species = self species 
		and: [ self name = aNode name
		and: [ aNode name notNil ] ]
]

{ #category : #accessing }
IceTipTreeGroup >> name [
	^ name
]

{ #category : #accessing }
IceTipTreeGroup >> name: anObject [
	name := anObject
]

{ #category : #accessing }
IceTipTreeGroup >> traversingDo: aBlock [
	^ self traversingDo: aBlock parents: #() 
]

{ #category : #accessing }
IceTipTreeGroup >> traversingDo: aBlock parents: anArray [
	aBlock cull: self cull: anArray.
	self children do: [ :each | 
		each 
			traversingDo: aBlock 
			parents: (anArray copyWith: self) ]
]
