serializing
initializeWithClassName: classString
classIsMeta: metaBoolean
selector: selectorString
category: catString
timeStamp: timeString
source: sourceString
	className := classString asSymbol.
	selector := selectorString asSymbol.
	category := catString asSymbol.
	timeStamp := timeString.
	classIsMeta := metaBoolean.
	source := sourceString withSqueakLineEndings.
