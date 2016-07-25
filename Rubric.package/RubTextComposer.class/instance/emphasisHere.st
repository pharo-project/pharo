accessing
emphasisHere
	^ emphasisHere ifNil: [ emphasisHere := ' ' asText attributesAt: 1 forStyle: self textStyle ]