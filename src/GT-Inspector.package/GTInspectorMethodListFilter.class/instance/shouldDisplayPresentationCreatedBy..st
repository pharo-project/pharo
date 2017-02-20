testing
shouldDisplayPresentationCreatedBy: aMethod

	^ selectedMethodSignatures anySatisfy: [ :aSignature | 
		aSignature first = aMethod methodClass and: [ aSignature second = aMethod selector ] ]