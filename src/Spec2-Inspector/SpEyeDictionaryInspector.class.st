"
Specialized version of inspector showing keys on left panel and values on description panel
"
Class {
	#name : #SpEyeDictionaryInspector,
	#superclass : #SpEyeCollectionInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #list }
SpEyeDictionaryInspector >> addVariableFields: elements [

	self sortedKeys do: [ :key |
		elements add: (SpIndexedEyeElement host: self object index: key)]
]

{ #category : #accessing }
SpEyeDictionaryInspector >> sortedKeys [

	^ [ [ self object keys sort ] 
		on: Error 
		do: [ "case of non sortable keys" self object keys ]
			] on: Error do: [ #() ]
]
