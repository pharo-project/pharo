testing
testBecome
	"Test the two way become. Note. we cannot use string literals for this test"
	| a b c d |

	a := 'ab' copy.
	b := 'cd' copy.
	c := a.
	d := b.

	a become: b.

	self 
		assert: a = 'cd';
		assert: b = 'ab';
		assert: c = 'cd';
		assert: d = 'ab'.


