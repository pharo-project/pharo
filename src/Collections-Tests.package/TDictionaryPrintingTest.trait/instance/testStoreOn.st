tests - printing
testStoreOn
	| str |
	str := String new writeStream.
	self nonEmptyDict storeOn: str.
	self assert: str contents = '((Dictionary new) add: (#b->30); add: (#c->1); add: (#d->-2); add: (#a->1); yourself)'