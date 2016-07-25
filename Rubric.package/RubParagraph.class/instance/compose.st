composition
compose
	self
		disableDrawingWhile: [ 
			self uptodateComposer 
				composeLinesFrom: 1
				to: self text size
				delta: 0
				into: OrderedCollection new
				priorLines: Array new
				atY: self compositionRectangle top.
			textArea ifNotNil: [ textArea paragraphWasComposedFrom: 1 to: self text size ] ]