parsing
grabAddresses
	"grab all the addresses in the string"
	| token |
	"remove comments"
	tokens removeAllSuchThat: [:t | t type == #Comment].
	"grab one address or address group each time through this loop"
	[ 
		"remove commas"
		[
			tokens isEmpty not and: [ tokens last type = $, ]
		] whileTrue: [ tokens removeLast ].

		"check whether any tokens are left"
		tokens isEmpty 
	] whileFalse: [
		token := tokens last.

		"delegate, depending on what form the address is in"
		"the from can be determined from the last token"

		token type = $> ifTrue: [
			self grabAddressWithRoute ]
		ifFalse: [ 
			(#(Atom DomainLiteral QuotedString) includes: token type)  ifTrue: [
				self grabBasicAddress ]
		ifFalse: [
			token type = $; ifTrue: [
				self grabGroupAddress ]
		ifFalse: [
			^self error: 'un-recognized address format' ] ] ]
	].

	^addresses