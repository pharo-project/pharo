private
updateLayoutInDockingBar

	owner isVertical
		ifFalse: [
			self hResizing: #shrinkWrap.
			self vResizing: #spaceFill]
		ifTrue: [
			self hResizing: #spaceFill.
			self vResizing: #shrinkWrap].
	self extent: self minWidth @ self minHeight