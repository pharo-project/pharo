Extension { #name : #DateAndTime }

{ #category : #'*metacello-core' }
DateAndTime >> setTimestampInMetacelloConfig: aMetacelloConfig [
    aMetacelloConfig setTimestampWithString: self printString
]

{ #category : #'*metacello-core' }
DateAndTime >> setTimestampInMetacelloVersion: aMetacelloVersionSpec [

	aMetacelloVersionSpec setTimestamp: 
		(aMetacelloVersionSpec project valueHolderSpec
			value: self printString;
			yourself)
]
