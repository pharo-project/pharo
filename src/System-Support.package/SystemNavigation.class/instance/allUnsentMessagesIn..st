query
allUnsentMessagesIn: selectorSet 
	"Answer the subset of selectorSet which are not sent anywhere in the 
	system. As allSentMessages does not recurse into nested Arrays, filter
	with #allCallsOn, which does"
	
	^ (selectorSet copyWithoutAll: self allSentMessages) select: [:each | (self allCallsOn: each) isEmpty]   