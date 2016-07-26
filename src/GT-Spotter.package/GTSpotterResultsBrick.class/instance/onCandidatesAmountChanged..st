event handling
onCandidatesAmountChanged: ann
	|categoryMorph|
	
	categoryMorph := self categoryBrickOf: ann category.
	categoryMorph setAmountVisible: (categoryMorph submorphs size) all: ann amount.
	