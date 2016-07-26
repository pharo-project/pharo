tests
testReadFrom
	"This is related to http://bugs.squeak.org/view.php?id=6779"
	
	self should: [(ScaledDecimal readFrom: '5.3') isKindOf: ScaledDecimal]
		description: 'Reading a ScaledDecimal should answer a ScaledDecimal'.
	self should: [((ScaledDecimal readFrom: '5.3') asScaledDecimal: 1) = (53/10 asScaledDecimal: 1)]
		description: 'ScaledDecimal readFrom: should not use Float intermediate because it would introduce round off errors'.