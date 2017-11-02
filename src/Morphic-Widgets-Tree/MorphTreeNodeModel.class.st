"
I wrap an item to be displayed in a TreeMorph, so I can answer content and representation in a polymorphic way. 

My hierarchy usually follows TreeMorphModel one.
"
Class {
	#name : #MorphTreeNodeModel,
	#superclass : #ListItemWrapper,
	#instVars : [
		'parentNode'
	],
	#category : 'Morphic-Widgets-Tree'
}

{ #category : #dependents }
MorphTreeNodeModel >> addDependent: aDependent [
	"Ensure to only have one dependent at every moment"
	
	dependents := #().
	super addDependent: aDependent
]

{ #category : #accessing }
MorphTreeNodeModel >> childNodeClassFromItem: anItem [
	^ self class
]

{ #category : #accessing }
MorphTreeNodeModel >> childNodeFromItem: anItem [
	^ ((self childNodeClassFromItem: anItem) with: anItem model: model) parentNode: self; yourself
]

{ #category : #accessing }
MorphTreeNodeModel >> childrenItems [
	^ Array new
]

{ #category : #accessing }
MorphTreeNodeModel >> color [
	^ nil
]

{ #category : #accessing }
MorphTreeNodeModel >> contents [
	^ self childrenItems collect: [:ci | self childNodeFromItem: ci ]
]

{ #category : #accessing }
MorphTreeNodeModel >> enabled [
	^ true
]

{ #category : #testing }
MorphTreeNodeModel >> hasContentToShow [

	^ false
]

{ #category : #accessing }
MorphTreeNodeModel >> helpText [
	^ model helpText
]

{ #category : #accessing }
MorphTreeNodeModel >> includesSubstringAnywhere: aString [
	^ (Array with: self asString) includesSubstringAnywhere: aString
]

{ #category : #protocol }
MorphTreeNodeModel >> isExpanded [

	^ false
]

{ #category : #protocol }
MorphTreeNodeModel >> isExpanded: aBoolean [
]

{ #category : #testing }
MorphTreeNodeModel >> isPartialMatch [
	| result |

	result := false.
	
	self contents do: [:each | 
		(each selected or: [ each isPartialMatch ]) ifTrue: [ result := true ] ].

	^ result
]

{ #category : #protocol }
MorphTreeNodeModel >> lastClicked: aBoolean [
	"Do nothing but introduce a hook"
]

{ #category : #accessing }
MorphTreeNodeModel >> level [
	^ parentNode ifNil: [1] ifNotNil: [parentNode level + 1]
]

{ #category : #accessing }
MorphTreeNodeModel >> model: anObject [
	model := anObject
]

{ #category : #events }
MorphTreeNodeModel >> mouseDown: event [
	"Do not do anything"
]

{ #category : #accessing }
MorphTreeNodeModel >> name [
	^ self item name
]

{ #category : #accessing }
MorphTreeNodeModel >> parentNode [
	^ parentNode
]

{ #category : #accessing }
MorphTreeNodeModel >> parentNode: aNode [
	parentNode := aNode
]

{ #category : #accessing }
MorphTreeNodeModel >> path [
	^ self pathIn: OrderedCollection new.
]

{ #category : #accessing }
MorphTreeNodeModel >> pathIn: aCollection [ 
	self parentNode
		ifNotNil: [(aCollection includes: self parentNode)
		ifFalse: [self parentNode pathIn: aCollection]].
	aCollection add: self.
	^ aCollection
]

{ #category : #printing }
MorphTreeNodeModel >> printOn: aStream [
	aStream nextPutAll: 'Node('.
	self item printOn: aStream.
	aStream nextPut: $)
]

{ #category : #accessing }
MorphTreeNodeModel >> rowMorphForColumn: aTreeColumn [
	^ self item ifNotNil: [:i | self asString asMorph] 
]

{ #category : #protocol }
MorphTreeNodeModel >> selected [

	^ false
]

{ #category : #protocol }
MorphTreeNodeModel >> selected: aBoolean [
	"Do nothing but introduce a hook"
]
