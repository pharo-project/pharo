enumerating
reject: block
	"Execute block for each of my elements and return a new instance of me 
	containing those elements for which the block returned false."

	| result |
	result := self class new.
	self do: [ :each |
		(block value: each)
			ifFalse: [ result add: each ] ].
	^ result