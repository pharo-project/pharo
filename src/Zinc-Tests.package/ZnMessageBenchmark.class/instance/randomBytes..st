private
randomBytes: count
	| bytes |
	bytes := ByteArray new: count.
	1 to: count do: [ :each | 
		bytes at: each put: (#(65 66 67 68) at: each \\ 4 + 1) ].
	^ bytes