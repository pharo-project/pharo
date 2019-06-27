"
Specialized version of inspector showing the size of the inspected collection in title.
"
Class {
	#name : #EyeCollectionInspector,
	#superclass : #EyeInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #accessing }
EyeCollectionInspector class >> label [
	^ 'Collection Inspector'
]

{ #category : #list }
EyeCollectionInspector >> addAllInstVars: elements [
	"Do nothing"
]

{ #category : #list }
EyeCollectionInspector >> addInstancesVariable: elements [
	"Do nothing"
]

{ #category : #list }
EyeCollectionInspector >> addVariableFields: elements [
	"add the variable fields to the inspector. Shorten the list for very long collection (cf limit1 and limit2), set them so that you can modify and save them"

	self variableFieldsToShow do: [ :index |
		elements add: (IndexedEyeElement host: self object index: index) ]
]

{ #category : #accessing }
EyeCollectionInspector >> objectVariableSize [
	^ [ self object size ] on: Error do: [ self object basicSize ]
]

{ #category : #accessing }
EyeCollectionInspector >> title [
	^ super title, ' [', self objectVariableSize asString, ']' 
]
