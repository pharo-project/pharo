accessing
at: anIndex put: anObject
	| basicSize counter |
	anIndex > 0 ifTrue: [
		basicSize := self basicSize.
		anIndex <= basicSize ifTrue: [
			counter := 0.
			1 to: basicSize do: [:i |
				(self basicAt: i) == nil
					ifFalse: [(counter := counter + 1) = anIndex ifTrue: [^self basicAt: i put: anObject]]]]].
	self error: 'access with an index out of bounds'