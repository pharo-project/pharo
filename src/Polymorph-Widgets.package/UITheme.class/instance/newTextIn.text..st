morph creation
newTextIn: aThemedMorph text: aStringOrText
	"Answer a new text."

	^TextMorph new
		wrapFlag: true;
		contents: aStringOrText;
		font: self textFont;
		autoFit: true;
		lock;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap