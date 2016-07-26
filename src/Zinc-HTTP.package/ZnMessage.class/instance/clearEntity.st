accessing
clearEntity
	"Set my entity to nil. 
	Unlike #resetEntity:, my content type and length remain unchanged."

	entity ifNotNil: [
		entity close.
		entity := nil ]