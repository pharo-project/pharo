tests - dictionary keys values associations access
testAssociations

	| collection result  |
	collection := self nonEmpty .
	result := collection associations.
	
	self assert: result size = collection size.
	result do: [:assoc | self assert: (assoc value) = (collection at: assoc key) ].
	"keys do: [ :key | self assert: ( result at: key ) = ( collection at: key )] ."
	