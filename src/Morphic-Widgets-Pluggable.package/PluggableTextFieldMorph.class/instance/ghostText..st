accessing
ghostText: aText
	"Set the value of helpText"
	ghostText := aText ifNotNil: [aText asString]
