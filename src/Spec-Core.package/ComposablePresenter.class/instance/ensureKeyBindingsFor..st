private-focus
ensureKeyBindingsFor: aWidget

	self registerKeyStrokesForNextFor: aWidget.
	self registerKeyStrokesForPreviousFor: aWidget.
	
	additionalKeyBindings associationsDo: [:association | aWidget bindKeyCombination: association key toAction: association value ]