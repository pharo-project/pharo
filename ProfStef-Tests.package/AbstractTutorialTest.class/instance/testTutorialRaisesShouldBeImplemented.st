tests
testTutorialRaisesShouldBeImplemented
	| tutorial |
	tutorial := AbstractTutorial new.
	self 
		should: [tutorial tutorial]
		raise: Error
		withExceptionDo: [:anException | 
			self 
				assert:  ShouldBeImplemented 
				equals: anException class
		].
	