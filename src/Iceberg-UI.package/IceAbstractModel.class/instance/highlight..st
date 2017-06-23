highlighting
highlight: aString
	[ 
		| text modified incoming outgoing |	
		text := Text fromString: aString.

		self repository isMissing ifTrue: [ ^ text allBold; makeAllColor: self colorError ].
		self isLoaded ifFalse: [ ^ text makeAllColor: self colorNotLoaded ].

		modified := entity isModified.
		incoming := self incomingCommits notEmpty.
		outgoing := self outgoingCommits notEmpty.

		modified ifTrue: [ text prepend: '* ' ].
		modified | incoming | outgoing ifTrue: [ text allBold ].

		modified ifTrue: [ ^ text makeAllColor: self colorModified ].
		incoming ifTrue: [ ^ text makeAllColor: self colorIncoming ].
		outgoing ifTrue: [ ^ text makeAllColor: self colorOutgoing ].
		^ text
	]
	on: Error do: [ ^ aString ]