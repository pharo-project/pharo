"
Specialized version of inspector showing values only
"
Class {
	#name : #SpEyeSetInspector,
	#superclass : #SpEyeCollectionInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #list }
SpEyeSetInspector >> addVariableFields: elements [
	"add the variable fields to the inspector. Shorten the list for very long collection (cf limit1 and limit2), set them so that you can modify and save them"
	| externalIndex |
	externalIndex := 1.
	self object array withIndexDo: [ :value :index |
		value ifNotNil: [ 
			elements add: (SpSetEyeElement host: self object index: index externalIndex: externalIndex).
			externalIndex := externalIndex + 1 ]]
]
