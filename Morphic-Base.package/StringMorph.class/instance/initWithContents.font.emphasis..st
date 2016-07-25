initialization
initWithContents: aString font: aFont emphasis: emphasisCode 
	super initialize.
	
	font := aFont.
	emphasis := emphasisCode.
	hasFocus := false.
	self contents: aString