accessing
approxMessagesOf: aMethod
	| messages |
	messages := IdentitySet new.
	self
		forApproxMessagesOf: aMethod
		do: [ :msg | messages add: msg ].
		
	^ messages 