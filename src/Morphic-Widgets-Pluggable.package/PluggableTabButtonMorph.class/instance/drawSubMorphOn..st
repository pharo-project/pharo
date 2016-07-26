drawing
drawSubMorphOn: aCanvas
	| morphBounds |
	morphBounds := self bounds insetBy: (self cornerRadius + 3) @ (self topInactiveGap // 2 + 2).
	morphBounds := morphBounds translateBy: 0@(self topInactiveGap // 2 + 1).
	self active ifTrue: [
		morphBounds := morphBounds translateBy: 0@((self topInactiveGap // 2 + 1) negated)].
	self subMorph bounds height < (morphBounds height)
		ifTrue: [
			morphBounds := morphBounds
				insetBy: 0@((morphBounds height - self subMorph bounds height) // 2)].
	self subMorph bounds width < (morphBounds width)
		ifTrue: [
			morphBounds := morphBounds
				insetBy: ((morphBounds width - self subMorph bounds width) // 2)@0].

	self subMorph bounds: morphBounds.			
	aCanvas drawMorph: self subMorph