tests
testTime

	self assertSerializationEqualityOf: (Time fromSeconds: 84072).
	self assertSerializationEqualityOf: (Time hour: 24 minute: 60 second: 60).
	self assertSerializationEqualityOf: (Time hour: 23 minute: 59 second: 59).
	self assertSerializationEqualityOf: (Time hour: 0 minute: 0 second: 0).
	self assertSerializationEqualityOf: (Time seconds: 0 nanoSeconds: 5).
	self assertSerializationEqualityOf: (Time allInstances sort:  [:a :b | a asSeconds > b asSeconds]) first.
	self assertSerializationEqualityOf: (Time allInstances sort:  [:a :b | a nanoSecond > b nanoSecond]) first
		
