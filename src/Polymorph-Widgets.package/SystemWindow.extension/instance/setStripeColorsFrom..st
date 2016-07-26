*Polymorph-Widgets
setStripeColorsFrom: paneColor 
	"Set the stripe color based on the given paneColor.
	Removed box color update for Pharo compatability."

	stripes ifNil: [^self].
	self fillStyle: self fillStyleToUse.
	self isActive 
		ifTrue: [label ifNotNil: [label color: self activeLabelFillStyle].
				labelArea fillStyle: self activeTitleFillStyle] 
		ifFalse: [label ifNotNil: [label color: self inactiveLabelFillStyle].
				labelArea fillStyle: self inactiveTitleFillStyle].
	self adoptPaneColor: self paneColor