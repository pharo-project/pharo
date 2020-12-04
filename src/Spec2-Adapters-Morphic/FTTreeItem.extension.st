Extension { #name : #FTTreeItem }

{ #category : #'*Spec2-Adapters-Morphic' }
FTTreeItem >> hasComputedChildren [
	"I return true if I have computed children or if I don't have computed children."

	^ self hasChildren
]
