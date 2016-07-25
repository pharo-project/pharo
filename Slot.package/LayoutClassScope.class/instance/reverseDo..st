enumerating
reverseDo: aBlock
	|size|
	size := self size.
	1 to: size do: [ :index |
		aBlock value: (self at: (1 + size - index)) ]