accessing
empty
	1 to: size do: [ :index |
		keys at: index put: nil.
		values at: index put: nil ].
	size := 0
