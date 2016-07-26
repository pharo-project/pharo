private
restoreValues
	| valueStream context |
	valueStream := values readStream.
	[valueStream atEnd] whileFalse:
		[context := valueStream next.
		1 to: context class instSize do: [:i | context instVarAt: i put: valueStream next].
		1 to: context size do: [:i | context at: i put: valueStream next]]