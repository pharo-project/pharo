accessing
entity: object
	"Set my entity to object. Unless I already am describing an entity using my content type and length
	set my content type and length as defined by object."

	entity := object.
	self headers isDescribingEntity ifFalse: [
		self headers acceptEntityDescription: object ]