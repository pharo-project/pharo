special instance creation
identity: n
	| r |
	r := self zeros: n.
	1 to: n do: [ :i | r at: i at: i put: 1 ].
	^ r