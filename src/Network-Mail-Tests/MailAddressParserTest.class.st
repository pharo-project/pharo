"
This is the unit test for the class MailAddressParser. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 
	- http://www.c2.com/cgi/wiki?UnitTest
	- there is a chapter in the PharoByExample book (http://pharobyexample.org)
	- the sunit class category
"
Class {
	#name : #MailAddressParserTest,
	#superclass : #TestCase,
	#category : #'Network-Mail-Tests'
}

{ #category : #tests }
MailAddressParserTest >> testAddressesAndNamePairsIn [

	| address |
	address := MailAddressParser addressesAndNamePairsIn: 'first last <person@company.com>'.
	self assert: address size equals: 1.
	self assert: address first equals: #('first last' 'person@company.com')
]

{ #category : #tests }
MailAddressParserTest >> testAddressesIn [

	| testString correctAnswer |

	testString := 'joe@lama.com, joe2@lama.com joe3@lama.com joe4 , Not an Address <joe5@address>, joe.(annoying (nested) comment)literal@[1.2.3.4], "an annoying" group : joe1@groupie, joe2@groupie, "Joey" joe3@groupy, "joe6"."joe8"@group.com;,  Lex''s email account <lex>'.

correctAnswer := #('joe@lama.com' 'joe2@lama.com' 'joe3@lama.com' 'joe4' 'joe5@address' 'joe.literal@[1.2.3.4]' 'joe1@groupie' 'joe2@groupie' '"Joey"' 'joe3@groupy' '"joe6"."joe8"@group.com' 'lex') asOrderedCollection.

	self assert: (MailAddressParser addressesIn: testString) equals:  correctAnswer
]

{ #category : #tests }
MailAddressParserTest >> testAmpersand [

	| testString correctAnswer |

	testString := '&@example.com, romeo&julia@verona.it'.

	correctAnswer := #('&@example.com' 'romeo&julia@verona.it') asOrderedCollection.

	self assert: (MailAddressParser addressesIn: testString) equals: correctAnswer
]
