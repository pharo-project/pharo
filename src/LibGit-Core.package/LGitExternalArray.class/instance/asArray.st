converting
asArray
	| array |
	array := Array new: self size.
	1 to: self size do: [ :index |
		array
			at: index
			put: (self at: index) ].
		
	^ array