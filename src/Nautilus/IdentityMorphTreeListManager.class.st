"
An IdentityMorphTreeListManager is a MorphTreeListManager which always check for identity between items

"
Class {
	#name : #IdentityMorphTreeListManager,
	#superclass : #MorphTreeListManager,
	#category : #'Nautilus-MorphTreeWidget-Extension'
}

{ #category : #accessing }
IdentityMorphTreeListManager >> nodeMorphsWithAllNodeItems: aNodeItemList [

	^ self allNodeMorphs select: [:m | aNodeItemList identityIncludes: m complexContents withoutListWrapper]
]
