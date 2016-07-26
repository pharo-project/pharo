member lookup
closestMemberWithStretchValue: stretchValue weightValue: weightValue slantValue: slantValue
	"answer the member that has weight, slant and stretch values that most closely 
	match those given by stretchValue, weightValue, and slantValue"
	
	^(self members asSortedCollection:[:a :b | 
		a isCloserMatchThan: b 
			toStretch: stretchValue 
			weight: weightValue 
			slant: slantValue]) first. 