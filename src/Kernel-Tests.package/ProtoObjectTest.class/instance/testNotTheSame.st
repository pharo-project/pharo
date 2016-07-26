tests - testing
testNotTheSame

	| object1 object2 |
	object1 := ProtoObject new.
	object2 := ProtoObject new.
	self assert: object1 ~~ object2.
	self deny: object1 ~~ object1.