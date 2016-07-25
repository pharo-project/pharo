comparing
= aRBClass 
	^self class = aRBClass class 
		and: [self name = aRBClass name and: [self model = aRBClass model]]