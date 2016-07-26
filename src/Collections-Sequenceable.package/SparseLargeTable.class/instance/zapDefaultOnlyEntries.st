accessing
zapDefaultOnlyEntries

	1 to: self basicSize do: [:i |
		(self allDefaultValueSubtableAt: i) ifTrue: [self basicAt: i put: nil].
	].
