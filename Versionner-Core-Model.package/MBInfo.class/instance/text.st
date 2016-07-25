accessing
text
	text ifNil: [ text := self buildStringOrText ].
	^text