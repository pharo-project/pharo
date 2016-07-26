accessing
acceptsSelection
	"this flag is used to specify whether the presentation
	is allowed to populate the selection port or not"
	^ acceptsSelection ifNil: [acceptsSelection := true]