converting
withSqueakLineEndings
	"Answer a copy of myself in which all sequences of <CR><LF> or <LF> have been changed to <CR>"
	| newText |
	(string includes: Character lf) ifFalse: [ ^self copy ].
	newText := self copyReplaceAll: String crlf with: String cr.
	(newText asString includes: Character lf) ifFalse: [ ^newText ].
	^newText copyReplaceAll: String lf with: String cr.