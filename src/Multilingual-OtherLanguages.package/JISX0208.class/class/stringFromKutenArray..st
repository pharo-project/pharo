class methods
stringFromKutenArray: anArray

	| s |
	s := WideString new: anArray size.
	1 to: anArray size do: [:i |
		s at: i put: (self charAtKuten: (anArray at: i)).
	].
	^s.
