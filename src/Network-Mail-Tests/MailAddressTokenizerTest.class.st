Class {
	#name : #MailAddressTokenizerTest,
	#superclass : #TestCase,
	#category : #'Network-Mail-Tests'
}

{ #category : #tests }
MailAddressTokenizerTest >> testTokensInAscii [
	"Basic test that the tokenizer returns the correct values"

	| tokens tokensCollection |

	tokens := MailAddressTokenizer tokensIn: 'first last <person@company.com>'.
	tokensCollection := tokens collect: [ :each |
		each type -> each text ] as: Array.
	self 
		assert: tokensCollection
		equals: {#Atom->'first'. #Atom->'last'. $<->'<'. #Atom->'person'. $@->'@'. #Atom->'company'. $.->'.'. #Atom->'com'. $>->'>'}
]

{ #category : #tests }
MailAddressTokenizerTest >> testTokensInLatin [
	"RFC6531 (February 2012) allows character values > 127.
	Check that the parser handles values between 128 and 255 (which can be correctly represented with single byte encoding)"

	| email tokens tokensCollection |

	email := 'first last <person@company.com>'.
	email
		at: 2 put: 237 asCharacter;
		at: 14 put: 233 asCharacter.
	tokens := MailAddressTokenizer tokensIn: email.
	tokensCollection := tokens collect: [ :each |
		each type -> each text ] as: Array.
	self 
		assert: tokensCollection
		equals: {
			#Atom->('first'
				at: 2 put: 237 asCharacter;
				yourself). 
			#Atom->'last'. 
			$<->'<'. 
			#Atom->('person'
				at: 2 put: 233 asCharacter;
				yourself). 
			$@->'@'. 
			#Atom->'company'. 
			$.->'.'. 
			#Atom->'com'. 
			$>->'>'}
]

{ #category : #tests }
MailAddressTokenizerTest >> testTokensInUnicode [
	"RFC6531 (February 2012) allows character values > 127.
	Check that the parser handles values greater than 255 (which require multi-byte encoding)"

	| email tokens tokensCollection |

	email := 'first last <person@company.com>'.
	email
		at: 3 put: 345 asCharacter;
		at: 15 put: 345 asCharacter.
	tokens := MailAddressTokenizer tokensIn: email.
	tokensCollection := tokens collect: [ :each |
		each type -> each text ] as: Array.
	self 
		assert: tokensCollection
		equals: {
			#Atom->('first'
				at: 3 put: 345 asCharacter;
				yourself). 
			#Atom->'last'. 
			$<->'<'. 
			#Atom->('person'
				at: 3 put: 345 asCharacter;
				yourself). 
			$@->'@'. 
			#Atom->'company'. 
			$.->'.'. 
			#Atom->'com'. 
			$>->'>'}
]
