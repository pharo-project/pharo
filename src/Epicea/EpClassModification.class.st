"
I represent the modification of a class.
"
Class {
	#name : #EpClassModification,
	#superclass : #EpClassChange,
	#instVars : [
		'oldClass',
		'newClass'
	],
	#category : #'Epicea-Model'
}

{ #category : #'instance creation' }
EpClassModification class >> oldClass: anOldClass newClass: aNewClass [

	^ self basicNew
		initializeOldClass: anOldClass
		newClass: aNewClass;
		yourself

]

{ #category : #visitor }
EpClassModification >> accept: aVisitor [

	^ aVisitor visitClassModification: self
]

{ #category : #accessing }
EpClassModification >> behaviorAffected [
	^ newClass
]

{ #category : #accessing }
EpClassModification >> behaviorAffectedName [
	^ newClass name
]

{ #category : #testing }
EpClassModification >> hasMetaclassChanges [
	"FIX should compare metaclasses as a whole"

	^ oldClass theMetaClass instVarNames ~= newClass theMetaClass instVarNames
]

{ #category : #testing }
EpClassModification >> hasNonMetaclassChanges [

	^ oldClass classVarNames ~= newClass classVarNames
		or: [ oldClass instVarNames ~= newClass instVarNames
				or: [ oldClass sharedPoolNames ~= newClass sharedPoolNames
						or: [ oldClass superclass name ~= oldClass superclass name ] ] ]
]

{ #category : #initialization }
EpClassModification >> initializeOldClass: anOldClass newClass: aNewClass [

	self initialize.
	oldClass := anOldClass asEpiceaRingDefinition theNonMetaClass.
	newClass := aNewClass asEpiceaRingDefinition theNonMetaClass.
]

{ #category : #accessing }
EpClassModification >> newClass [

	^ newClass
]

{ #category : #accessing }
EpClassModification >> oldClass [

	^ oldClass
]
