test - integrity
testHealthyWorks
	"we use associations as keys on purpose, because they changing
	hash depending on the key"
	| a1 a2 dict |
	self classToBeTested = Dictionary
		ifFalse: [  ^ self "only works on Dictionary" ].
	dict := Dictionary new.
	[a1 := 1 -> 2.
	a2 := 2 -> 2.
	dict 
		at: a1 put: 2;
		at: a2 put: 3.
	self assert: dict isHealthy.
	a1 key: 0.
	a2 key: 0.
	self assert: dict isHealthy not]
		ensure: [dict removeAll]