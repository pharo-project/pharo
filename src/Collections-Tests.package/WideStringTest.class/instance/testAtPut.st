testing
testAtPut
	"Non regression test for http://bugs.squeak.org/view.php?id=6998"
	
	| w1 |
	w1 := WideString with: (Unicode value: 402) with: $a with: (Unicode value: 400) with: $b.
	self assert: (w1 at: 2 put: $b) = $b description: 'at:put: should return the put-object'
