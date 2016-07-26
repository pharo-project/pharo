tests
testCadrMatching
	"self debug: #testCadrMatching"

	"A bit more complex example is the following expression, matching the
name of any of the Lisp-style `car', `cdr', `caar', `cadr',
... functions:"

	self assert: ( 'car' matchesRegex: 'c(a|d)+r').
	self assert: ( 'cdr' matchesRegex: 'c(a|d)+r').
	self assert: ( 'caar' matchesRegex: 'c(a|d)+r').
	self assert: ( 'cadr' matchesRegex: 'c(a|d)+r').
	self assert: ( 'caddar' matchesRegex: 'c(a|d)+r').