"
I am a special kind of hierarchy which merges multiple different hierarchies.
Each of my #mergedParts adds own relationship to the building hiearchy map: 

	ClyMergedHierarchy>>buildParentMap: aHierarchyMap for: aClass
		mergedParts do: [ :each | 
			aHierarchyMap hierarchy: each.
			each buildParentMap: aHierarchyMap for: aClass ].
		aHierarchyMap hierarchy: self.

To create my instances use following methods:
	
	ClyMergedHierarchy merge: {ClySuperclassHierarchy new. ClyTraitUserHierarchy new}.

Or simply use comma to concatenate hierarchies	:
	
	ClySubclassHierarchy new, ClyTraitUserHierarchy new.

In this example the parent of each class can be either superclass or inherited trait.
Such hierarchy can include duplicated nodes.

To provide consisten merging I override sort function of all my parts to ensure that all of them are sorted in same way.

Internal Representation and Key Implementation Points.

    Instance Variables
	mergedParts:		<Collection of<ClyClassHierarchy>>
"
Class {
	#name : #ClyMergedHierarchy,
	#superclass : #ClyClassHierarchy,
	#instVars : [
		'mergedParts'
	],
	#category : #'Calypso-SystemQueries-Results'
}

{ #category : #'instance creation' }
ClyMergedHierarchy class >> merge: hierarchies [
	^self new 
		mergedParts: hierarchies
]

{ #category : #comparing }
ClyMergedHierarchy >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	super = anObject ifFalse: [ ^ false ].
	^ mergedParts = mergedParts 
]

{ #category : #building }
ClyMergedHierarchy >> buildParentMap: aHierarchyMap for: aClass [

	mergedParts do: [ :each | 
		aHierarchyMap hierarchy: each.
		each buildParentMap: aHierarchyMap for: aClass ].
	
	aHierarchyMap hierarchy: self.
]

{ #category : #comparing }
ClyMergedHierarchy >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ super hash bitXor: mergedParts hash
]

{ #category : #initialization }
ClyMergedHierarchy >> initialize [
	super initialize.
	mergedParts := #()
]

{ #category : #accessing }
ClyMergedHierarchy >> mergedParts [
	^ mergedParts
]

{ #category : #accessing }
ClyMergedHierarchy >> mergedParts: anObject [
	mergedParts := anObject
]

{ #category : #accessing }
ClyMergedHierarchy >> sortFunction: aSortFunction [
	super sortFunction: aSortFunction.
	mergedParts do: [:each | each sortFunction: aSortFunction]
]
