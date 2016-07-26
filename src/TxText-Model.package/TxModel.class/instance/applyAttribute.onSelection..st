private
applyAttribute: anAttribute onSelection: selection
	"Preserve the selection interval while modifying the text"
	
	| currentSpan normalizedSelection |
	selection isEmpty ifTrue: [ ^ self ].
	normalizedSelection := selection normalized.
	currentSpan := normalizedSelection start span.
	[ 
		currentSpan
			applyAttribute: anAttribute
			from: normalizedSelection start
			to: normalizedSelection end.
		currentSpan == normalizedSelection end span ifTrue: [ ^ self normalize: selection. "done" ].
		currentSpan := currentSpan next.
	] repeat.
	
	