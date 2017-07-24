private
copyFrom: aPath
	| size |
	size := aPath size min: self size.
	1 to: size do: [:i | self at: i put: (aPath at: i)].
	