private
recalculateVerticalScrollBarVisibilityIfHidden: aBlock
	self container calculateExactVisibleRows >= self dataSource numberOfRows
		ifTrue: [ 
			(self isVerticalScrollBarVisible)
				ifTrue: [ self removeMorph: self verticalScrollBar ].
			self resizeContainer. "it changed... I need to resize it immediately because 
			otherwise it does not work fine with first show... this can cause two sends to 
			#resizeContainer but the case is minimal and not expensive, so we can ignore it"
			aBlock value ]
		ifFalse: [ 
			(self isVerticalScrollBarVisible)
				ifFalse: [ 
					self resizeVerticalScrollBar.
					self addMorph: self verticalScrollBar ] ]
