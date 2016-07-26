examples
smalltalkCodeWithStabilo
	"self smalltalkCodeWithStabilo"
	| area seg code |
	area := RubEditingArea new
		beWrapped;
		width: 400;
		updateTextWith: (code := self someSmallSmalltalkCode).
	area addSegment: (seg := RubPlainSegmentMorph inTextArea: area from: 3 to: code size).
	seg color: (Color yellow).
	seg borderColor: (Color orange alpha: 0.5).
	seg beSmoothCurve.
	area openInWorld