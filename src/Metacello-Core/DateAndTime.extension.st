Extension { #name : 'DateAndTime' }

{ #category : '*Metacello-Core' }
DateAndTime >> setTimestampInMetacelloConfig: aMetacelloConfig [

	self printString setTimestampInMetacelloConfig: aMetacelloConfig
]

{ #category : '*Metacello-Core' }
DateAndTime >> setTimestampInMetacelloVersion: aMetacelloVersionSpec [

	aMetacelloVersionSpec setTimestamp: 
		(aMetacelloVersionSpec project valueHolderSpec
			value: self printString;
			yourself)
]
