tests
testIntervalStoreOn
	"this is a test for http://bugs.squeak.org/view.php?id=4378"
	
	| interval1 interval2 |
	interval1 := 0 to: 1 by: 0.5s1 squared.
	interval2 := self class compiler evaluate: interval1 storeString.
	self assert: interval1 size = interval2 size