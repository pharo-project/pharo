tests - printing
testStoreOn
	"self debug: #testStoreOn"
	
	self assert: self nonEmptyDict storeString = ('((', self nonEmptyDict class printString , ' new) add: (#a->1); add: (#b->30); add: (#c->1); add: (#d->-2); yourself)')