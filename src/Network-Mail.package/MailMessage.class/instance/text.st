accessing
text
	"the full, unprocessed text of the message"
	text ifNil: [ self regenerateText ].
	^text