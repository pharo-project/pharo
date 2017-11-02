"
I am the bridget between a TreeNodePresenter and a SpecTreeNodeModel
"
Class {
	#name : #MorphicTreeNodeAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : 'Spec-MorphicAdapters'
}

{ #category : #factory }
MorphicTreeNodeAdapter >> buildWidget [
	^ SpecTreeNodeModel new
		setItemFromBlock: self itemBlock;
		icon: self icon;
		setChildren: self childrenBlock;
		hasContents: self hasChildrenBlock;
		model: self container;
		nodeModel: self model
]

{ #category : #'widget API' }
MorphicTreeNodeAdapter >> childrenBlock [
	^ [ self model buildChildren ]
]

{ #category : #'widget API' }
MorphicTreeNodeAdapter >> container [
	^ self model container
]

{ #category : #private }
MorphicTreeNodeAdapter >> deselect [

	self widgetDo: [ :w |
		w changed: #deselect.
		w model selectionChanged.
		w dependents do: [ :e | e changed ] ]
]

{ #category : #'widget API' }
MorphicTreeNodeAdapter >> hasChildrenBlock [
	^ self model hasChildren
]

{ #category : #'widget API' }
MorphicTreeNodeAdapter >> hasContentToShow [
	^ self model hasContentToShow
]

{ #category : #'widget API' }
MorphicTreeNodeAdapter >> icon [
	^ self model icon
]

{ #category : #'spec protocol' }
MorphicTreeNodeAdapter >> isExpanded: aBoolean [

	
]

{ #category : #'widget API' }
MorphicTreeNodeAdapter >> item [
	^ self model content
]

{ #category : #'widget API' }
MorphicTreeNodeAdapter >> itemBlock [
	^ [ self item ]
]

{ #category : #private }
MorphicTreeNodeAdapter >> select [

	self widgetDo: [ :w |
		w changed: #select.
		w model selectionChanged.
		w dependents do: [ :e | e changed ] ]
]

{ #category : #'widget API' }
MorphicTreeNodeAdapter >> selected [
	^ self model selected
]

{ #category : #'spec protocol' }
MorphicTreeNodeAdapter >> selected: aBoolean [

	aBoolean 
		ifTrue: [ self select ]
		ifFalse: [ self deselect ]
]

{ #category : #'spec protocol' }
MorphicTreeNodeAdapter >> takeHighlight [

	self widgetDo: [ :w | w changed: #takeHighlight ]
]

{ #category : #'spec protocol' }
MorphicTreeNodeAdapter >> updateAccordingTo: aFilter [

	self widgetDo: [ :w |
		w contents do: [ :n | n nodeModel updateAccordingTo: aFilter ].
		w contents removeAllSuchThat: [ :n | (aFilter keepTreeNode: n nodeModel) not  and: [ n nodeModel isEmpty ] ] ]
]
