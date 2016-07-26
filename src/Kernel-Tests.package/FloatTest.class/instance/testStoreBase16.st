tests - printing
testStoreBase16
	"This bug was reported in mantis http://bugs.squeak.org/view.php?id=6695"

	self
		assert: (20.0 storeStringBase: 16) = '16r14.0'
		description: 'the radix prefix should not be omitted, except in base 10'