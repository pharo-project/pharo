"
I am the bridget between a TreeNodePresenter and a SpecTreeNodeModel
"
Class {
	#name : #SpMorphicTreeNodeAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Deprecated-Adapters'
}

{ #category : #deprecation }
SpMorphicTreeNodeAdapter class >> isDeprecated [ 

	^ true
]

{ #category : #factory }
SpMorphicTreeNodeAdapter >> buildWidget [
	^ SpTreeNodeModel new
		setItemFromBlock: self itemBlock;
		icon: self icon;
		setChildren: self childrenBlock;
		hasContents: self hasChildrenBlock;
		model: self container;
		nodeModel: self model
]

{ #category : #'widget API' }
SpMorphicTreeNodeAdapter >> childrenBlock [
	^ [ self model buildChildren ]
]

{ #category : #'widget API' }
SpMorphicTreeNodeAdapter >> container [
	^ self model container
]

{ #category : #private }
SpMorphicTreeNodeAdapter >> deselect [

	self widgetDo: [ :w |
		w changed: #deselect.
		w model selectionChanged.
		w dependents do: [ :e | e changed ] ]
]

{ #category : #'widget API' }
SpMorphicTreeNodeAdapter >> hasChildrenBlock [
	^ self model hasChildren
]

{ #category : #'widget API' }
SpMorphicTreeNodeAdapter >> hasContentToShow [
	^ self model hasContentToShow
]

{ #category : #'widget API' }
SpMorphicTreeNodeAdapter >> icon [
	^ self model icon
]

{ #category : #'spec protocol' }
SpMorphicTreeNodeAdapter >> isExpanded: aBoolean [

	
]

{ #category : #'widget API' }
SpMorphicTreeNodeAdapter >> item [
	^ self model content
]

{ #category : #'widget API' }
SpMorphicTreeNodeAdapter >> itemBlock [
	^ [ self item ]
]

{ #category : #private }
SpMorphicTreeNodeAdapter >> select [

	self widgetDo: [ :w |
		w changed: #select.
		w model selectionChanged.
		w dependents do: [ :e | e changed ] ]
]

{ #category : #'widget API' }
SpMorphicTreeNodeAdapter >> selected [
	^ self model selected
]

{ #category : #'spec protocol' }
SpMorphicTreeNodeAdapter >> selected: aBoolean [

	aBoolean 
		ifTrue: [ self select ]
		ifFalse: [ self deselect ]
]

{ #category : #'spec protocol' }
SpMorphicTreeNodeAdapter >> takeHighlight [

	self widgetDo: [ :w | w changed: #takeHighlight ]
]

{ #category : #'spec protocol' }
SpMorphicTreeNodeAdapter >> updateAccordingTo: aFilter [

	self widgetDo: [ :w |
		w contents do: [ :n | n nodeModel updateAccordingTo: aFilter ].
		w contents removeAllSuchThat: [ :n | (aFilter keepTreeNode: n nodeModel) not  and: [ n nodeModel isEmpty ] ] ]
]
