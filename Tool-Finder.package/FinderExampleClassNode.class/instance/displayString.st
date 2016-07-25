private
displayString
	(self parentNode receiver isKindOf: self item)
		ifTrue: [ ^ '*' , super displayString ].
	^ super displayString