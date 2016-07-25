canvas accessing
contentsOfArea: aRectangle into: aForm
	"this may conflict with current pathtransformations, but up to now"
	"it works"
	| bb |
	self flush.
	bb := BitBlt toForm: aForm.
	bb
		sourceForm: self form;
		combinationRule: Form over;
		sourceX: aRectangle left + origin x;
		sourceY: aRectangle top + origin y;
		width: aRectangle width;
		height: aRectangle height;
		copyBits.
	^ aForm