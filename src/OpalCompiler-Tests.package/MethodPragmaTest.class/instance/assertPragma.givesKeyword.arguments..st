utilities
assertPragma: aString givesKeyword: aSymbol arguments: anArray
	| pragma  |
	pragma := self pragma: aString selector: #zork.
	self assert: pragma keyword = aSymbol.
	self assert: pragma arguments = anArray.
