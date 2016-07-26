private
string: aString
	1 to: aString size do: [:j | 
		self privateAt: j put: (aString at: j)].
	^self