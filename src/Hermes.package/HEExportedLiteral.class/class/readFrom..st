instance creation
readFrom: aReader
	| tag subclass |
	tag := aReader readByte.
	subclass := self allSubclasses detect: [ :e | e tag = tag ].
	^ subclass new
		doReadFrom: aReader;
		yourself