fields
removeFieldNamed: name
	"remove all fields with the specified name"
	fields removeKey: name ifAbsent: []