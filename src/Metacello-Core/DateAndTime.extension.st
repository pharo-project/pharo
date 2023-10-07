Extension { #name : 'DateAndTime' }

{ #category : '*Metacello-Core' }
DateAndTime >> setTimestampInMetacelloConfig: aMetacelloConfig [
    aMetacelloConfig setTimestampWithString: self printString
]

{ #category : '*Metacello-Core' }
DateAndTime >> setTimestampInMetacelloVersion: aMetacelloVersionSpec [

	aMetacelloVersionSpec setTimestamp: 
		(aMetacelloVersionSpec project valueHolderSpec
			value: self printString;
			yourself)
]
