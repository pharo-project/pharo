accessing
at: anIndex
	| basicSize counter dep |
	anIndex > 0 ifTrue: [
		basicSize := self basicSize.
		anIndex <= basicSize ifTrue: [
			counter := 0.
			1 to: basicSize do: [:i |
				(dep := self basicAt: i) == nil
					ifFalse: [(counter := counter + 1) = anIndex ifTrue: [^dep]]]]].
	self error: 'access with an index out of bounds'