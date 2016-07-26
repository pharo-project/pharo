simple ensure tests
doOnDo: aBlock 
	
	x := 5.
	[aBlock value.
	self ensure] on: Error do:  [:ex | self ensure].