examples
exampleFullEmphaisedString
	<sampleInstance>
	| font |
	font := LogicalFont familyName: StandardFonts defaultFont familyName pointSize: 42. 
	^ (self contents: 'This is a StringMorph with emphasis 2r11111' font: font emphasis: 2r11111) 
			position: 100@100;
			backgroundColor: Color orange;
			openInWorld