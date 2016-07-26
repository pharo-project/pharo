comparing
= anObject
	^ anObject species == self species 
		and: [receiver == anObject receiver
		and: [selector == anObject selector
		and: [arguments = anObject arguments]]]