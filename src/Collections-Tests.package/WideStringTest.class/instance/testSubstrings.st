tests - substrings
testSubstrings
	"this is related to http://bugs.squeak.org/view.php?id=6367"
	
	| w1 w2 |
	w1 := WideString with: 401 asCharacter with: $a with: 402 asCharacter with: $b.
	w2 := WideString with: 403 asCharacter with: 404 asCharacter.

	self assert: w1 substrings first = w1.
	self assert: (w1 , ' ' , w2) substrings size = 2.
	self assert: (w1 , ' ' , w2) substrings last = w2.