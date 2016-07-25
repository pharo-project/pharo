*Polymorph-Widgets
setExpandBoxBalloonText
	"Set the expand box balloon help text as appropriate."

	expandBox ifNil: [^self].
	self unexpandedFrame
		ifNil: [expandBox setBalloonText: 'Expand to full screen' translated]
		ifNotNil: [expandBox setBalloonText: 'Contract to original size' translated]