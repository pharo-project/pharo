printing
printMetacelloProjectDetails: metacelloProject
	| versions |
	self inform: 'Available versions for ', self configurationName.
	
	versions := metacelloProject symbolicVersionMap keys asSet.
	versions addAll: metacelloProject map keys.
	versions add: #last.
	
	"print the found version sorted alphabetically"
	versions asArray sorted do:[ :key|
		self stdout nextPutAll: key; lf ]