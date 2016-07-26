tests
testEqual
	"(self run: #testEqual)"
	"(self debug: #testEqual)"
	| bag1 bag2 |
	bag1 := Bag new.
	bag2 := Bag new.
	self assert: bag1 = bag2.
	bag1 add: #a;
		 add: #b.
	bag2 add: #a;
		 add: #a.
	self deny: bag1 = bag2.
	self assert: bag1 = bag1.
	bag1 add: #a.
	bag2 add: #b.
	self assert: bag1 = bag2.
	bag1 add: #c.
	self deny: bag1 = bag2.
	bag2 add: #c.
	self assert: bag1 = bag2