copying
postCopy

	super postCopy.
	keys := keys copy.
	values := values copy.
	1 to: 4096 do: [ :index |
		(keys at: index) ifNotNil: [ :list |
			keys at: index put: list copy.
			values at: index put: (values at: index) copy ] ]