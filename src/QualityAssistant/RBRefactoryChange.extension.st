Extension { #name : #RBRefactoryChange }

{ #category : #'*qualityAssistant' }
RBRefactoryChange >> diffedText [

	^ TextDiffBuilder 
			buildDisplayPatchFrom: self oldVersionTextToDisplay 
			to: self textToDisplay
]
