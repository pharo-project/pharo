accessing
problemCount
	^rules inject: 0 into: [:count :each | count + each problemCount]