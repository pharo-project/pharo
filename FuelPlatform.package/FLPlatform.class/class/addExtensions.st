private-extensions
addExtensions
	self extensionPragmas do: [ :pragma |
		(FLExtensionWrapper fromPragma: pragma) install ]