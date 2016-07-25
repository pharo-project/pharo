instance creation
defaultConverterClassForEncoding: encodingName
	"TextConverter defaultConverterClassForEncoding: 'shift-jis'"

	^ self allSubclasses
		detect: [:class | class encodingNames includes: encodingName]
		ifNone: []
