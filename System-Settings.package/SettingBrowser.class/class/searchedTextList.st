accessing
searchedTextList
	"Keep a list of previously entered filtering tokens"
	^ searchedTextList ifNil: [searchedTextList := OrderedCollection new]