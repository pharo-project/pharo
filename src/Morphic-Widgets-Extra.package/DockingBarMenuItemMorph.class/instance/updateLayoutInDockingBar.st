private
updateLayoutInDockingBar

	owner isVertical
		ifTrue: [
			self hResizing: #spaceFill.
			self vResizing: #shrinkWrap]
		ifFalse: [
			self hResizing: #shrinkWrap.
			self vResizing: #spaceFill].
	self extent: self minWidth @ self minHeight