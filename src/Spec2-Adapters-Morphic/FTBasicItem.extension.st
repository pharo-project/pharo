Extension { #name : #FTBasicItem }

{ #category : #'*Spec2-Adapters-Morphic' }
FTBasicItem >> hasComputedChildren [
	"I return true if I have computed children or if I don't have computed children."

	^ children ifNil: [ true ] ifNotNil: [ super hasComputedChildren ]
]
