tests
testStoreOn
	"this is http://bugs.squeak.org/view.php?id=4378"
	
	"Both results should be 1.
	ScaledDecimal representations are exact
	(though only scale digits or fractional part are printed)"

	self assert:
    		(self class compiler evaluate: (0.5s1 squared storeString)) * 4
		= (0.5s1 squared * 4).
		
	
	"However, exact literals should store literaly
	If not, they would break Decompiler."
	
	"BUG: i cannot write the test like this:
	self assert:
    		0.5s2 squared storeString = '0.25s2'
	BECAUSE compiler would consider 0.5s2 as = 0.5s1 and would reuse same slot..."
	
	self assert:
    		0.25s2 storeString = '0.25s2'