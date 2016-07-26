instance creation
withAll: aCollection
	| newInstance |
	newInstance := self basicNew: aCollection size.
	1 to: aCollection size do: [:i |
		newInstance basicAt: i put: (aCollection at: i)].
	^newInstance