accessing
font: aFont emphasis: emphasisCode
	font := aFont.
	emphasis := emphasisCode.
	self fitContents.
"
in inspector say,
	 self font: (TextStyle default fontAt: 2) emphasis: 1
"