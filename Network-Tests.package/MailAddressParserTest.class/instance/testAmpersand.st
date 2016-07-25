tests
testAmpersand

	| testString correctAnswer |

	testString := '&@example.com, romeo&julia@verona.it'.

	correctAnswer := #('&@example.com' 'romeo&julia@verona.it') asOrderedCollection.

	self assert: (MailAddressParser addressesIn: testString) equals: correctAnswer