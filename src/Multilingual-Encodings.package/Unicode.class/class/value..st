instance creation
value: code

	code < 256 ifTrue: [^ Character value: code].
	"Old code. LeadingChar should from now on be always 0, so we can remove it afterwards"
	^ Character leadingChar: 0 code: code