private
finishedReading: aReadOnlyFileArray from: aQueue

	(aQueue == readOnlyQueue and: [ aQueue size < self readOnlyQueueLimit ]) 
		ifTrue: [ aQueue nextPut: aReadOnlyFileArray ]
		ifFalse: [ self closeFileArray: aReadOnlyFileArray ]