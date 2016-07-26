accessing
fontNamed: fontName  "TextStyle default fontNamed: 'TimesRoman10'"
	^ fontArray detect: [:x | x name sameAs: fontName]