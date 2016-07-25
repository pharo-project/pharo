drawing
drawLinesOn: aCanvas
	"Draw the lines for the submorphs.
	Modified for performance."
	
	| lColor|
	lColor := self lineColor.
	aCanvas 
		transformBy: scroller transform
		clippingTo: scroller innerBounds
		during: [:clippedCanvas |
			scroller submorphs do: [ :submorph | | last |
				((submorph isExpanded and: [
					(submorph nextSibling notNil and: [
						clippedCanvas isVisible: (submorph fullBounds topLeft
							corner: submorph nextSibling fullBounds bottomRight)]) or: [
					submorph nextSibling isNil and: [(last := submorph lastChild) notNil and: [
						clippedCanvas isVisible: (submorph fullBounds topLeft
							corner: last fullBounds bottomRight)]]]]) or: [
				(clippedCanvas isVisible: submorph fullBounds) or: [
				(submorph nextSibling notNil and: [
						clippedCanvas isVisible: submorph nextSibling fullBounds])]]) ifTrue:[
					submorph drawLinesOn: clippedCanvas lineColor: lColor]]]
		smoothing: scroller smoothing
