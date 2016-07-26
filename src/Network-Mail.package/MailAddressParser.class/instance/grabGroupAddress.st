parsing
grabGroupAddress
	"grab an address of the form 'phrase : address, address, ..., address;'"
	"I'm not 100% sure what this format means, so I'm just returningthe list of addresses between the : and ;   -ls  (if this sounds right to someone, feel free to remove this comment :)"

	"remove the $; "
	tokens removeLast.

	"grab one address each time through this loop"
	[ 
		"remove commas"
		[
			tokens isEmpty not and: [ tokens last type = $, ]
		] whileTrue: [ tokens removeLast ].

		tokens isEmpty ifTrue: [
			"no matching :"
			^self error: 'stray ; in address list'. ].

		tokens last type = $:
	] whileFalse: [
		"delegate to either grabAddressWithRoute, or grabBasicAddress.  nested groups are not allowed"

		tokens last type = $> ifTrue: [
			self grabAddressWithRoute ]
		ifFalse: [ 
			(#(Atom DomainLiteral QuotedString) includes: tokens last type)  ifTrue: [
				self grabBasicAddress ]
		ifFalse: [
			^self error: 'un-recognized address format' ] ]
	].

	tokens removeLast.   "remove the :"

	self removePhrase.