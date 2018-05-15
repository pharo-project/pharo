"
This is the abstract class for a presentation that is supposed to show a list of elements. Thus, the displayValue should be a list.

Instance Variables:
	tagsBlock	<Object>
	searchBlock	<MocketryProxy | Object | SMProxy>
	filterBlock	<MocketryProxy | Object | SMProxy>
	amountToShowBlock	<Object>
	allowsMultipleSelection	<Boolean>
	tagsFilterBlock	<MocketryProxy | Object | SMProxy>
	allowsDeselection	<Boolean>
	tagsStyle	<String>
	helpMessage	<String>
"
Class {
	#name : #GLMListingPresentation,
	#superclass : #GLMFormatedPresentation,
	#instVars : [
		'searchBlock',
		'filterBlock',
		'amountToShowBlock',
		'allowsMultipleSelection',
		'allowsDeselection',
		'tagsStyle',
		'helpMessage',
		'sortBlock',
		'textBackgroundColor',
		'dropOnItemBlock',
		'allowDropOnItemBlock',
		'allowItemDragBlock',
		'transformDraggedItemBlock',
		'allowsRecursiveSearch'
	],
	#category : #'Glamour-Presentations'
}

{ #category : #'accessing values' }
GLMListingPresentation >> accept: passengerObject droppedOnItem: anItem [
	self dropOnItemBlock glamourValue: (GLMMultiValue with: passengerObject with: anItem with: self).
	^ true
]

{ #category : #'accessing values' }
GLMListingPresentation >> allow: passengerObject droppedOnItem: anItem [
	^ self allowDropOnItemBlock glamourValue: (GLMMultiValue with: passengerObject with: anItem with: self)
]

{ #category : #scripting }
GLMListingPresentation >> allowDeselection [
	self allowsDeselection: true
]

{ #category : #scripting }
GLMListingPresentation >> allowDropOnItem: aBlock [
	allowDropOnItemBlock := aBlock
]

{ #category : #accessing }
GLMListingPresentation >> allowDropOnItemBlock [
	^ allowDropOnItemBlock ifNil: [ allowDropOnItemBlock := [ false ] ]
]

{ #category : #scripting }
GLMListingPresentation >> allowItemDrag: aBlock [
	allowItemDragBlock := aBlock
]

{ #category : #accessing }
GLMListingPresentation >> allowItemDragBlock [
	^ allowItemDragBlock
]

{ #category : #accessing }
GLMListingPresentation >> allowsDeselection [
	^ allowsDeselection ifNil: [allowsDeselection := false]
]

{ #category : #accessing }
GLMListingPresentation >> allowsDeselection: anObject [
	allowsDeselection := anObject
]

{ #category : #testing }
GLMListingPresentation >> allowsFilter [
	^ filterBlock notNil
]

{ #category : #testing }
GLMListingPresentation >> allowsInput [
	^ self allowsFilter or: [ self allowsSearch ]
]

{ #category : #testing }
GLMListingPresentation >> allowsItemDrag [
	^ self allowItemDragBlock notNil
]

{ #category : #accessing }
GLMListingPresentation >> allowsRecursiveSearch [
	^ allowsRecursiveSearch ifNil: [allowsRecursiveSearch := true]
]

{ #category : #accessing }
GLMListingPresentation >> allowsRecursiveSearch: aBoolean [
	allowsRecursiveSearch := aBoolean
]

{ #category : #testing }
GLMListingPresentation >> allowsSearch [
	^ searchBlock notNil
]

{ #category : #accessing }
GLMListingPresentation >> amountToShow [
	^ (amountToShowBlock ifNil: [ [ self defaultAmountToShow ] ]) glamourValue: self entity
]

{ #category : #scripting }
GLMListingPresentation >> beMultiple [
	allowsMultipleSelection := true
]

{ #category : #scripting }
GLMListingPresentation >> beSingle [
	allowsMultipleSelection := false
]

{ #category : #accessing }
GLMListingPresentation >> childrenValueOf: anObject [
	self flag: 'this is rather hackish, but needed to treat list and tree in the same way.'.
	^ OrderedCollection new
]

{ #category : #accessing }
GLMListingPresentation >> childrenValueOf: anObject atLevel: anInteger [
	self flag: 'this is rather hackish, but needed to treat list and tree in the same way.'.
	^ OrderedCollection new
]

{ #category : #accessing }
GLMListingPresentation >> columns [
	self flag: 'this is rather hackish, but needed to treat list and tree in the same way.'.
	^ OrderedCollection new
]

{ #category : #tags }
GLMListingPresentation >> computeTagsFilterFor: aGlamourValue [ 
	self subclassResponsibility 
]

{ #category : #tags }
GLMListingPresentation >> computeTagsFor: aGlamourValue [ 
	self subclassResponsibility 
]

{ #category : #accessing }
GLMListingPresentation >> defaultAmountToShow [
	^ 500
]

{ #category : #'accessing values' }
GLMListingPresentation >> displayValue [
	^ sortBlock 
		ifNil: [super displayValue]
		ifNotNil: [super displayValue sorted: sortBlock]
]

{ #category : #scripting }
GLMListingPresentation >> dropOnItem: aBlock [
	dropOnItemBlock := aBlock
]

{ #category : #accessing }
GLMListingPresentation >> dropOnItemBlock [
	^ dropOnItemBlock 
]

{ #category : #scripting }
GLMListingPresentation >> filterOn: aBlock [ 
	filterBlock := aBlock
]

{ #category : #accessing }
GLMListingPresentation >> filterStrategy [
	^filterBlock
]

{ #category : #accessing }
GLMListingPresentation >> grayRoundedTags [

	tagsStyle := #grayRoundedButton
]

{ #category : #accessing }
GLMListingPresentation >> helpMessage [

	^ helpMessage ifNil: [
		helpMessage := 'Search Input'
		]

	
]

{ #category : #accessing }
GLMListingPresentation >> helpMessage: aString [

	helpMessage := aString.

	
]

{ #category : #testing }
GLMListingPresentation >> isDragSource [ 
	^ self dropOnItemBlock notNil
]

{ #category : #testing }
GLMListingPresentation >> isDropTarget [
	^ self dropOnItemBlock notNil
]

{ #category : #testing }
GLMListingPresentation >> isMultiple [
	^ allowsMultipleSelection ifNil: [ allowsMultipleSelection := false ]
]

{ #category : #testing }
GLMListingPresentation >> isSingle [ 
	^ self isMultiple not
]

{ #category : #scripting }
GLMListingPresentation >> searchOn: aBlock [
	searchBlock := aBlock
]

{ #category : #accessing }
GLMListingPresentation >> searchStrategy [
	^ searchBlock
]

{ #category : #accessing }
GLMListingPresentation >> selectionPath [
	^ (self pane port: #selectionPath) value
]

{ #category : #accessing }
GLMListingPresentation >> selectionPath: anObject [
	(self pane port: #selectionPath) value: anObject
]

{ #category : #scripting }
GLMListingPresentation >> showOnly: aBlock [
	amountToShowBlock := aBlock
]

{ #category : #accessing }
GLMListingPresentation >> sortBlock [
	^ sortBlock
]

{ #category : #accessing }
GLMListingPresentation >> sortBlock: anObject [
	sortBlock := anObject
]

{ #category : #scripting }
GLMListingPresentation >> sorted [
	sortBlock := [:a :b | a < b]
]

{ #category : #scripting }
GLMListingPresentation >> sorted: aBlock [
	sortBlock := aBlock  
]

{ #category : #tags }
GLMListingPresentation >> tagsFor: anObject fromProvider: aTagProvider to: operation [
	| glamourValue |
	
	glamourValue := (anObject asGlamorousMultiValue, self entity asGlamorousMultiValue).
	^ operation == #show 
		ifTrue: [ aTagProvider computeTagsFor: glamourValue ] 
		ifFalse: [ aTagProvider computeTagsFilterFor: glamourValue ]
]

{ #category : #tags }
GLMListingPresentation >> tagsFor: anObject to: operation [
	
	^ self 
		tagsFor: anObject 
		fromProvider: self 
		to: operation
]

{ #category : #accessing }
GLMListingPresentation >> tagsStyle [

	^tagsStyle ifNil:[ self grayRoundedTags. tagsStyle ]
]

{ #category : #accessing }
GLMListingPresentation >> textBackgroundColor [

	^ textBackgroundColor ifNil: [textBackgroundColor := Color transparent]
]

{ #category : #accessing }
GLMListingPresentation >> textBackgroundColor:  aBlockOrAColor [

	textBackgroundColor := aBlockOrAColor
]

{ #category : #accessing }
GLMListingPresentation >> textBackgroundColorFor: anObject [
	^ self textBackgroundColor glamourValue: (anObject asGlamorousMultiValue, self entity asGlamorousMultiValue)
]

{ #category : #scripting }
GLMListingPresentation >> transformDraggedItem: aBlock [
	transformDraggedItemBlock := aBlock
]

{ #category : #accessing }
GLMListingPresentation >> transformDraggedItemBlock [
	^ transformDraggedItemBlock
]

{ #category : #'accessing values' }
GLMListingPresentation >> transformedDraggedItem: anItem [
	^ self transformDraggedItemBlock 
		ifNotNil: [ :transform | transform glamourValue: (GLMMultiValue with: anItem with: self)]
		ifNil: [ anItem ]
]

{ #category : #accessing }
GLMListingPresentation >> whiteRectangledTags [

	tagsStyle := #whiteRectangledButton
]

{ #category : #scripting }
GLMListingPresentation >> withSmalltalkSearch [
	self
		searchOn: [:text :each | 
			Smalltalk compiler receiver: each; evaluate: ' | entity each | each := self. entity := each.', text];
		helpMessage: 'Quick selection field. Given your INPUT, it executes: self select: [:each | INPUT ]'
]
