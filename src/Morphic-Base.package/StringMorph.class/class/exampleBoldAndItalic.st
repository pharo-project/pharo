examples
exampleBoldAndItalic
	<sampleInstance>
	
	| font |
	font := LogicalFont familyName: StandardFonts defaultFont familyName pointSize: 42. 
	^ (self contents: 'This is a StringMorph with emphasis bold and italic' font: font emphasis: TextEmphasis bold)
			addEmphasis: TextEmphasis italic;
			position: 100@100;
			backgroundColor: Color orange;
			openInWorld