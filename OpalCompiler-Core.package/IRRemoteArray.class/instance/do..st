enumerating
do: aBlock
	1 to: size do: [ :idx | aBlock value: (idx - 1) ]