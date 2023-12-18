"
I represent the modification of a class.
"
Class {
	#name : 'EpClassModification',
	#superclass : 'EpClassChange',
	#instVars : [
		'oldClass',
		'newClass'
	],
	#category : 'Epicea-Model',
	#package : 'Epicea',
	#tag : 'Model'
}

{ #category : 'instance creation' }
EpClassModification class >> oldClass: anOldClass newClass: aNewClass [

	^ self basicNew
		initializeOldClass: anOldClass
		newClass: aNewClass;
		yourself
]

{ #category : 'visitor' }
EpClassModification >> accept: aVisitor [

	^ aVisitor visitClassModification: self
]

{ #category : 'accessing' }
EpClassModification >> behaviorAffected [
	^ newClass
]

{ #category : 'accessing' }
EpClassModification >> behaviorAffectedName [
	^ newClass name
]

{ #category : 'accessing' }
EpClassModification >> fileOutEntryOn: aWriteStream [
	"Write out class definition for the given class on the given stream, if the class definition was added or changed."

	| realClass |
	
	realClass := self newClass realClass.
	aWriteStream
		nextChunkPut: realClass definitionString;
		cr.

]

{ #category : 'testing' }
EpClassModification >> hasChanges [
	^ oldClass definitionSource ~= newClass definitionSource or: [
		oldClass classSide definitionSource ~= newClass classSide definitionSource ]
]

{ #category : 'testing' }
EpClassModification >> hasMetaclassChanges [
	"FIX should compare metaclasses as a whole"

	^ oldClass classSide instVarNames ~= newClass classSide instVarNames
]

{ #category : 'testing' }
EpClassModification >> hasNonMetaclassChanges [

	^ oldClass classVarNames ~= newClass classVarNames
		or: [ oldClass instVarNames ~= newClass instVarNames
				or: [ oldClass sharedPoolNames ~= newClass sharedPoolNames
						or: [ oldClass superclassName ~= newClass superclassName ] ] ]
]

{ #category : 'initialization' }
EpClassModification >> initializeOldClass: anOldClass newClass: aNewClass [

	self initialize.
	oldClass := anOldClass asEpiceaRingDefinition instanceSide.
	newClass := aNewClass asEpiceaRingDefinition instanceSide
]

{ #category : 'accessing' }
EpClassModification >> newClass [

	^ newClass
]

{ #category : 'accessing' }
EpClassModification >> oldClass [

	^ oldClass
]
