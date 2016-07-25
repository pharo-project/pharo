accessing
familyName: aName size: aSize
	"Answer a font (or the default font if the name is unknown) in the specified size."
	| style |
	style := TextStyle named: aName asSymbol.
	style ifNil: [^(FontSubstitutionDuringLoading forFamilyName: aName pixelSize: aSize)
			signal: 'missing font' ].
	^style fontOfSize: aSize