enumerating
select: block
	"Execute block for each of my elements and return a new instance of me 
	containing those elements for which the block returned true."

	| result |
	result := self class new.
	self do: [ :each |
		(block value: each)
			ifTrue: [ result add: each ] ].
	^ result