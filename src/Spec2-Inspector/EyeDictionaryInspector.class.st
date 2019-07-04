"
Specialized version of inspector showing keys on left panel and values on description panel
"
Class {
	#name : #EyeDictionaryInspector,
	#superclass : #EyeCollectionInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #list }
EyeDictionaryInspector >> addVariableFields: elements [

	self sortedKeys do: [ :key |
		elements add: (IndexedEyeElement host: self object index: key)]
]

{ #category : #accessing }
EyeDictionaryInspector >> sortedKeys [

	^ [ [ self object keys sort ] 
		on: Error 
		do: [ "case of non sortable keys" self object keys ]
			] on: Error do: [ #() ]
]
