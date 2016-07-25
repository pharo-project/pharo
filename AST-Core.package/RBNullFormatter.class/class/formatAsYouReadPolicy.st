initialize
formatAsYouReadPolicy
	"Return whether the formatter is continously formatting"

	^ FormatAsYouReadPolicy ifNil: [ FormatAsYouReadPolicy := false ]
