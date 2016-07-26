font accessing
printStandardSystemFonts
	"self printStandardSystemFonts"

	| string |
	string := String streamContents: [ :s |

	StandardFonts systemFontAspects do: [:selector |
		| font |
		font := StandardFonts perform: selector.
		s
			nextPutAll: selector; space;
			nextPutAll: font familyName; space;
			nextPutAll: (AbstractFont emphasisStringFor: font emphasis);
			nextPutAll: ' points: ';
			print: font pointSize;
			nextPutAll: ' height: ';
			print: font height;
			cr
		]].
	UIManager default  
		longMessage: string
		title: 'Current system font settings' translated.
	