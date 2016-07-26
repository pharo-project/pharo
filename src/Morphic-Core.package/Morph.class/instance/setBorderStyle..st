accessing
setBorderStyle: aSymbol
	"Set the border style of my costume"

	| aStyle |
	aStyle := self borderStyleForSymbol: aSymbol.
	aStyle ifNil: [^ self].
	(self canDrawBorder: aStyle)
		ifTrue:
			[self borderStyle: aStyle]