private
result: anObject

	lock critical: [ 
		hasResult := true.
		result := anObject ].