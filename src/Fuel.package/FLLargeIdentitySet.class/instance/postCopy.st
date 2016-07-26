copying
postCopy

	super postCopy.
	array := array copy.
	1 to: 4096 do: [ :index |
		(array at: index) ifNotNil: [ :list |
			array at: index put: list copy ] ]