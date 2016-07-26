accessing
description
	"Return a textual description of the exception."

	^ String
		streamContents: [ :stream | 
			stream << super description asString.
			stream cr.
			stream << self methodClass asString.
			stream cr.
			stream << self methodNode asString ]