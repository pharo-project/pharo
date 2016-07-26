accessing
resetEntity: object
	"Set my entity to object. Always set my content type and length as defined by object,
	even if I am already describing an entity using my content type and length.
	See also: #clearEntity"

	(entity ~= object and: [ entity notNil ]) 
		ifTrue: [ entity close ].
	entity := object.
	self headers acceptEntityDescription: object