accessing
autoAccept: aBoolean
	"Answer whether the editor accepts its contents on each change."

	autoAccept := aBoolean.
	self textMorph
		ifNotNil: [:t | t autoAccept: aBoolean]