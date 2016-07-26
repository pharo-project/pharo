private
string: aString

	1 to: aString size do: [:j | super at: j put: (aString at: j)].
	^self  