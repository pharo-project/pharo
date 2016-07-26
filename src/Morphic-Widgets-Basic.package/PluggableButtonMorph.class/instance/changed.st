updating
changed
	"Update the fillStyle here."

	|lc pc bs|
	self assureExtension.
	extension borderStyle: (bs := self borderStyleToUse).
	borderColor := bs style.
	borderWidth := bs width.
	extension fillStyle: self fillStyleToUse.
	self layoutInset: (self theme buttonLabelInsetFor: self).
	self privateColor: self fillStyle asColor.
	(self labelMorph respondsTo: #interactionState:)
		ifTrue: [self labelMorph interactionState: self interactionState]
		ifFalse: [(self labelMorph isNil
			or: [label isMorph]) ifFalse: [
				pc := self normalColor.
				lc := self enabled
					ifTrue: [pc contrastingColor]
					ifFalse: [pc contrastingColor muchDarker].
				self labelMorph color: lc]].
	super changed