tests - integrity
testIsHealthy
	| dict |
	self canBeUnhealthy
		ifFalse: [
			self assert: self nonEmpty isHealthy.
			^ self ].
	dict := self emptyDict.
	[ | a1 a2 |
	"we use associations as keys on purpose, because they change their
	hash depending on the key"
	a1 := 1 -> 2.
	a2 := 2 -> 2.
	dict
		at: a1 put: 2;
		at: a2 put: 3.
	self assert: dict isHealthy.
	a1 key: 0.
	a2 key: 0.
	self deny: dict isHealthy ]
		ensure: [ dict removeAll ]