*opalcompiler-core
asOneArgSelector
	^ String streamContents: [:str|
		| parts |
		parts := (self findTokens: ':)').
			str nextPutAll: parts first.
			parts allButFirst do: [:each | str nextPutAll: each capitalized]	.
			str nextPutAll: ':']