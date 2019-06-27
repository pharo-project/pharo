"
Specialized version of inspector showing the size of the inspected collection in title.
"
Class {
	#name : #SpEyeCollectionInspector,
	#superclass : #SpEyeInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #accessing }
SpEyeCollectionInspector class >> label [
	^ 'Collection Inspector'
]

{ #category : #list }
SpEyeCollectionInspector >> addAllInstVars: elements [
	"Do nothing"
]

{ #category : #list }
SpEyeCollectionInspector >> addInstancesVariable: elements [
	"Do nothing"
]

{ #category : #list }
SpEyeCollectionInspector >> addVariableFields: elements [
	"add the variable fields to the inspector. Shorten the list for very long collection (cf limit1 and limit2), set them so that you can modify and save them"

	self variableFieldsToShow do: [ :index |
		elements add: (SpIndexedEyeElement host: self object index: index) ]
]

{ #category : #accessing }
SpEyeCollectionInspector >> objectVariableSize [
	^ [ self object size ] on: Error do: [ self object basicSize ]
]

{ #category : #accessing }
SpEyeCollectionInspector >> title [
	^ super title, ' [', self objectVariableSize asString, ']' 
]
