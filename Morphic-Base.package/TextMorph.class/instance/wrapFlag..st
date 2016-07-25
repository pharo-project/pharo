accessing
wrapFlag: aBoolean
	"Should contained text stay wrapped at my width?"

	aBoolean == wrapFlag ifTrue: [^ self].
	wrapFlag := aBoolean.
	self composeToBounds