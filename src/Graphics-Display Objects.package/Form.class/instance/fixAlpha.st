other
fixAlpha
	"Fix the alpha channel if the receiver is 32bit"
	| bb |
	self depth = 32 ifFalse:[^self].
	bb := BitBlt toForm: self.
	bb combinationRule: 40 "fixAlpha:with:".
	bb copyBits.