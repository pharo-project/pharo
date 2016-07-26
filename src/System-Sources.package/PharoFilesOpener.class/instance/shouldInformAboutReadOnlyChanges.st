public
shouldInformAboutReadOnlyChanges
	"Answer true if and only if the user must be informed when the .changes file can not be written to."
	^ shouldInformAboutReadOnlyChanges ifNil: [ shouldInformAboutReadOnlyChanges := true ]