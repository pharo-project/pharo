accessing - extension
resetExtension
	"reset the extension slot if it is not needed"
	(extension notNil and: [extension isDefault]) ifTrue: [extension := nil] 