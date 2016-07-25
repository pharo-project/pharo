parsing
addressesIn: aString
	"return a collection of the bare addresses listed in aString"
	| tokens |
	tokens := MailAddressTokenizer tokensIn: aString.
	^(self new initialize: tokens) grabAddresses