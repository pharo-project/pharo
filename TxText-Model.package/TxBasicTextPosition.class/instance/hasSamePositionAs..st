comparing
hasSamePositionAs: position
	"Answer true if receiver having same position (in terms of text index) as another position.
	See #textIndex for explanation of text index"
	
	self span == position span ifTrue: [	^ self spanPosition = position spanPosition ].
		
	(self isAtSpanStart and: [ position isAtSpanEnd and: [ self span prev = position span]])
			ifTrue: [ ^ true ].

	(self isAtSpanEnd and: [ position isAtSpanStart and: [ self span next = position span]])
		ifTrue: [ ^ true ].
	
	^ false