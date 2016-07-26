comparing
analogousCodeTo: anObject
	"For MethodPropertires comparison."
	^self class == anObject class
	  and: [selector == anObject selector
	  and: [args = anObject arguments
	  and: [lookupClass == anObject lookupClass]]]