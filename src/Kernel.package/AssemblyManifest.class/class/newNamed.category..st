instance creation
newNamed: aString category: aCategoryName
	| assemblyClass |
	assemblyClass := self subclass: aString asSymbol
			instanceVariableNames: ''
			classVariableNames: ''
			poolDictionaries: ''
			category: aCategoryName.
	assemblyClass comment: self manifestClassComment.
	^ assemblyClass