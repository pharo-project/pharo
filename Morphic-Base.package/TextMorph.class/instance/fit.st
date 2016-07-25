private
fit
	"Adjust my bounds to fit the text.  Should be a no-op if autoFit is not specified.
	Required after the text changes,
	or if wrapFlag is true and the user attempts to change the extent."

	| newExtent para cBounds lastOfLines heightOfLast |
	self isAutoFit 
		ifTrue: 
			[newExtent := (self paragraph extent max: 9 @ textStyle lineGrid) + (0 @ 2).
			newExtent := newExtent + (2 * borderWidth).
			newExtent := ((0 @ 0 extent: newExtent) expandBy: margins) extent.
			newExtent ~= bounds extent 
				ifTrue: 
					[(container isNil and: [successor isNil]) 
						ifTrue: 
							[para := paragraph.	"Save para (layoutChanged smashes it)"
							super extent: newExtent.
							paragraph := para]].
			(container notNil and: [successor isNil])
				ifTrue: 
					[cBounds := container bounds truncated.
					"23 sept 2000 - try to allow vertical growth"
					lastOfLines := self paragraph lines last.
					heightOfLast := lastOfLines bottom - lastOfLines top.
					(lastOfLines last < text size 
						and: [lastOfLines bottom + heightOfLast >= self bottom]) 
							ifTrue: 
								[container releaseCachedState.
								cBounds := cBounds origin corner: cBounds corner + (0 @ heightOfLast)].
					self privateBounds: cBounds]].

	"These statements should be pushed back into senders"
	self paragraph positionWhenComposed: self position.
	successor ifNotNil: [successor predecessorChanged].
	self changed	"Too conservative: only paragraph composition
					should cause invalidation."