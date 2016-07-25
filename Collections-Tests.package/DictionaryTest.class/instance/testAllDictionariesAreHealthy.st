test - integrity
testAllDictionariesAreHealthy
	"only makes sense on Dictionary"
	self classToBeTested = Dictionary 
		ifFalse: [  ^ self ].
	self assert: (Dictionary allSubInstances select: [:dict | dict isHealthy not ]) isEmpty.