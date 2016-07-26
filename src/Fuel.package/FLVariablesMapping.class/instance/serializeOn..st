serialize/materialize
serializeOn: anEncoder

	anEncoder encodeByte: notIgnoredVariables size.	
	notIgnoredVariables do: [ :name | anEncoder encodeString: name ].