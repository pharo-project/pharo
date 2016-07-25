requirements
valueArray
" return a collection (with the same size than 'indexArray' )of values to be put in 'nonEmpty'  at indexes in 'indexArray' "
	| result |
	result := Array new: self indexArray size.
	1 to: result size do:
		[:i |
		result at:i put: (self aValue ).
		].
	^ result.