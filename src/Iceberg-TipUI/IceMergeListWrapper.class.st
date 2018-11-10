"
I am a morphic list wrapper that wraps iceberg diff tree nodes (potentially with conflicts)
"
Class {
	#name : #IceMergeListWrapper,
	#superclass : #ListItemWrapper,
	#category : #'Iceberg-TipUI-Merge'
}

{ #category : #comparing }
IceMergeListWrapper >> = aWrapper [

	^ self item = aWrapper item
]

{ #category : #'morphic-compatibility' }
IceMergeListWrapper >> actualClass [
	
	^ self item value actualClass
]

{ #category : #testing }
IceMergeListWrapper >> asString [

	^item value description
]

{ #category : #resolution }
IceMergeListWrapper >> chooseLocal [
	
	^ item value selectRight
]

{ #category : #resolution }
IceMergeListWrapper >> chooseRemote [
	
	^ item value selectLeft
]

{ #category : #testing }
IceMergeListWrapper >> contents [
	"Answer the contents of the change from the model."

	^ item children collect: [ :each | IceMergeListWrapper with: each model: model ]
]

{ #category : #comparing }
IceMergeListWrapper >> hash [

	^ self item hash
]

{ #category : #testing }
IceMergeListWrapper >> icon [

	^ item value icon
]

{ #category : #testing }
IceMergeListWrapper >> isConflict [
	
	^ item value isConflict
]

{ #category : #accessing }
IceMergeListWrapper >> preferredColor [

	^ self item value preferredColor
]
