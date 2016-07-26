morphic ui
orderSpecs
	^{
		'Unchanged' -> nil.
		'Order by package' -> [ :x :y | x first <= y first ].
		'Order by author' -> [ :x :y | x second <= y second ].
		'Order by version-string' -> [ :x :y | x third <= y third ].
		'Order by version-number' -> [ :x :y | x third asNumber >= y third asNumber ].
		'Order by filename' -> [ :x :y | x fourth <= y fourth ].
	}