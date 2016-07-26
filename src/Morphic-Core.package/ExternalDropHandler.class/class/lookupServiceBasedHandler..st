accessing
lookupServiceBasedHandler: dropStream
	"the file was just droped, let's do our job"
	| fileName services theOne |
	fileName := dropStream name.

	services := (Smalltalk tools fileList itemsForFile: fileName asFileReference)
		reject: [:svc | self unwantedSelectors includes: svc selector].

	"no service, default behavior"
	services isEmpty
		ifTrue: [^nil].

	theOne := self chooseServiceFrom: services.
	^theOne
		ifNotNil: [ExternalDropHandler type: nil extension: nil action: [:stream | theOne performServiceFor: stream]]