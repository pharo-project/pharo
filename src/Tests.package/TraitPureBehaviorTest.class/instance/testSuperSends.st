testing-applying trait composition
testSuperSends
	"self debug: #testSuperSends"
	
	| aC2 |
	aC2 := self c2 new.
	self assert: aC2 m51.
	self deny: aC2 foo.
	self deny: aC2 bar