*monticellofiletree-core
writeCypressJsonOn: aStream forHtml: forHtml indent: startIndent
	"by default ignore <forHtml> ... <forHtml> is used for Dictionary and Array, i.e., container objects and String which actually encodes itself differently for HTML"

	aStream nextPutAll: '"'.
	forHtml
		ifTrue: [ aStream nextPutAll: self cypressEscape ]
		ifFalse: [ self cypressEscapeCharactersOn: aStream ].
	aStream nextPutAll: '"'