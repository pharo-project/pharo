*qualityAssistant
diffedText

	^ TextDiffBuilder 
			buildDisplayPatchFrom: self oldVersionTextToDisplay 
			to: self textToDisplay