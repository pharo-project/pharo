as yet unclassified
wantsClick
	"Don't if we are transparent for now."

	^(self src color isTransparent and: [self dst color isTransparent]) not