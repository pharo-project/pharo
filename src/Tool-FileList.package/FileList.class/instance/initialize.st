initialization
initialize

	super initialize.
	ok := false.
	sourceTextModel := RubScrolledTextModel new interactionModel: self.
	dirSelectionBlock := [ :dirName | true].
	baseLabel := ''.
	configuredServices := OrderedCollection new.
	self directory: FileSystem workingDirectory.