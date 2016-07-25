private
allDefaultValueSubtableAt: index

	| t |
	t := self basicAt: index.
	t ifNil: [^ true].
	t do: [:e |
		e ~= defaultValue ifTrue: [^ false].
	].
	^ true.
