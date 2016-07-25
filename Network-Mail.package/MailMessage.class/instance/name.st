accessing
name
	"return a default name for this part, if any was specified.  If not, return nil"
	| type nameField disposition |

	"try in the content-type: header"
	type := self fieldNamed: 'content-type' ifAbsent: [nil].
	(type notNil and: [(nameField := type parameters at: 'name' ifAbsent: [nil]) notNil])
		ifTrue: [^ nameField].

	"try in content-disposition:"
	disposition := self fieldNamed: 'content-disposition' ifAbsent: [nil].
	(disposition notNil and: [(nameField := disposition parameters at: 'filename' ifAbsent: [nil]) notNil])
		ifTrue: [^ nameField].

	"give up"
	^ nil