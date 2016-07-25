initialization
setField: fieldName to: aFieldValue
	"set a field.  If any field of the specified name exists, it will be overwritten"
	fields at: fieldName asLowercase put: (OrderedCollection with: aFieldValue).
	text := nil.