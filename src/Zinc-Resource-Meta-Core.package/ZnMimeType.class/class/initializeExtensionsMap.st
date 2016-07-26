private
initializeExtensionsMap
	"Create a mapping between filename extensions and mime types"

	ExtensionsMap := Dictionary new.
	self mimeTypeFilenameExtensionsSpec
		do: [ :each | 
			| mimeType |
			mimeType := self fromString: each first.
			each allButFirstDo: [ :extension | ExtensionsMap at: extension put: mimeType ] ].
	self patchExtensionsMap