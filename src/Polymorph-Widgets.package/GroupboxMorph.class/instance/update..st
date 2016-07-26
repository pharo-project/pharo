updating
update: aSymbol
	"Replace the content if appropriate."
	
	super update: aSymbol.
	aSymbol = self getContentSelector ifTrue: [
		self contentFromModel ifNotNil: [:c | self replaceContentMorph: c]]
		