*Collections-arithmetic
\\= other

	other isNumber ifTrue: [
		1 to: self size do: [:i |
			self at: i put: (self at: i) \\ other
		].
		^ self.
	].
	1 to: (self size min: other size) do: [:i |
		self at: i put: (self at: i) \\ (other at: i).
	].

