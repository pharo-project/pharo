enumerating
partsDo: block
	parts isNil ifTrue: [ ^ self ].
	parts do: block  