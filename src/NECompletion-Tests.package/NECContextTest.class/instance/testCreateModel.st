tests
testCreateModel
	| text context |
	text := 'testIt: aRectangle
	aRectangle printS'.
	context := self createContextFor: text at: text size.
	self assert: context createModel class == NECTypedModel.
	text := 'testIt:'.
	context := self createContextFor: text at: text size.
	NECPreferences overrideModel 
		ifTrue: [ self assert: context createModel class = NECOverrideModel]
		ifFalse: [self assert: context createModel class = NECEmptyModel].
	text := 'testIt: rect
	rect is'.
	context := self createContextFor: text at: text size.
	self assert: context createModel class = NECUntypedModel