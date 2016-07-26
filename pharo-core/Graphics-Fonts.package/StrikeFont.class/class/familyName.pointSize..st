accessing
familyName: aName pointSize: aSize
	"Answer a font (or the default font if the name is unknown) in the specified size."

	^ ((TextStyle named: aName asSymbol) ifNil: [TextStyle default]) fontOfPointSize: aSize