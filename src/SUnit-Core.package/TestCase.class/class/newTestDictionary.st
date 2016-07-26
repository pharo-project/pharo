history
newTestDictionary

	^ Dictionary new at: #timeStamp put: DateAndTime now;
		at: #passed put: Set new;
		at: #failures put: Set new;
		at: #errors put: Set new;
		yourself
		