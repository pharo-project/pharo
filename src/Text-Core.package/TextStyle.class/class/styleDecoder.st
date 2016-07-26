initialization
styleDecoder
	TextSharedInformation at: #StyleDecoder ifPresent: [ :dict | dict ifNotNil: [ ^dict ]].
	^TextSharedInformation at: #StyleDecoder put: (
		Dictionary new at: 'Regular' put: 0;
				 at: 'Roman' put: 0;
				 at: 'Medium' put: 0;
				 at: 'Light' put: 0;
				 at: 'Normal' put: 0;
				 at: 'Plain' put: 0;
				 at: 'Book' put: 0;
				 at: 'Demi' put: 0;
				 at: 'Demibold' put: 0;
				 at: 'Semibold' put: 0;
				 at: 'SemiBold' put: 0;
				 at: 'ExtraBold' put: 1;
				 at: 'SuperBold' put: 1;
				 at: 'B' put: 1;
				 at: 'I' put: 2;
				 at: 'U' put: 4;
				 at: 'X' put: 16;
				 at: 'N' put: 8;
				 at: 'Bold' put: 1;
				 at: 'Italic' put: 2;
				 at: 'Oblique' put: 2;
				 at: 'Narrow' put: 8;
				 at: 'Condensed' put: 8;
				 at: 'Underlined' put: 4;
				 yourself )