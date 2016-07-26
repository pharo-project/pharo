accessing
events
	| events |
	events := self unpackedData.
	events do: [ :each | self addInfoFor: each ].
	^ events