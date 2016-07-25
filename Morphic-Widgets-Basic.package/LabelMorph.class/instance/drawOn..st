drawing
drawOn: aCanvas
	"Draw based on enablement."

	super drawOn: aCanvas.

	(self enabled not and: [ self disabledStyle = #inset ])	ifTrue: [
		aCanvas
			drawString: self contents
			in: (self stringBounds translateBy: 1)
			font: self fontToUse
			color: self theme lightBackgroundColor.
	].




		
