highlighting
highlight: aString
	[ 
		| text modified |	
		text := Text fromString: aString.

		self repository isMissing ifTrue: [ ^ text allBold; makeAllColor: self colorError ].
		self isLoaded ifFalse: [ ^ text makeAllColor: self colorNotLoaded ].

		modified := entity isModified.
		modified ifTrue: [ text prepend: '* ' ].
		modified ifTrue: [ text allBold ].

		modified ifTrue: [ ^ text makeAllColor: self colorModified ].
		^ text
	]
	on: Error do: [ ^ aString ]