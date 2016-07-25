parsing
grabAddressWithRoute
	"grad an address of the form 'Descriptive Text <real.address@c.d.e>"
	
	self startNewAddress.

	tokens removeLast.	"remove the >"

	"grab until we see a $<"
	[ 
		tokens isEmpty ifTrue: [
			self error: '<> are not matched' ].
		tokens last type = $<
	] whileFalse: [ self addToAddress ].

	tokens removeLast.  "remove the <"


	self removePhrase.

	self finishAddress