private
fullDescription
	self categories ifEmpty: [ ^ self commonPrefix ].
	^ String
		streamContents: [ :stream | 
			| first |
			first := self categories first.
			stream << first.
			self categories allButFirst
				do: [ :c | 
					| part |
					part := c withoutPrefix: first.
					stream << $| << part ] ]