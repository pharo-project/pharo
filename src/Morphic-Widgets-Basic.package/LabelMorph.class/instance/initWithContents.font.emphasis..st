initialization
initWithContents: aString font: aFont emphasis: emphasisCode
	"Grrr, why do they do basicNew?"
	
	super initWithContents: aString font: aFont emphasis: emphasisCode.
	self
		disabledStyle: #plain;
		enabled: true