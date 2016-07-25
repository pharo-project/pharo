showing
whileNotDoneDo: aBlock
	[ work isEmpty ] whileFalse: [ aBlock value ]