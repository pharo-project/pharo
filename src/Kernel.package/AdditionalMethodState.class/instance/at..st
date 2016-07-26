accessing
at: aKey
	"Answer the property value or pragma associated with aKey."
	
	^self at: aKey ifAbsent: [self error: 'not found']