comparing
= anObject
	^ (anObject isKindOf: TxBasicTextPosition) and: [
		anObject text = self text and: [ anObject span = self span and: [ anObject spanPosition = self spanPosition ]]]