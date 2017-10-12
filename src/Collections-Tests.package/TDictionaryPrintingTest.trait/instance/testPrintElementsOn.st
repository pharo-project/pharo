tests - printing
testPrintElementsOn
	| str |
	str := String new writeStream.
	self nonEmptyDict printElementsOn: str.
	self assert: (str contents = '(#a->1 #b->30 #c->1 #d->-2 )')