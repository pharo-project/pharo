enumerating
collect: block
	"Execute block for each of my elements and return a new instance of me containing the results."
	
	| result |
	result := self class new.
	self do: [ :each |
		result add: (block value: each) ].
	^ result