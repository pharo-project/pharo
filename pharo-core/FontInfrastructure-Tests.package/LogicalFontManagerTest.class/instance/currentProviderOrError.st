accessing
currentProviderOrError
	| provider |
	provider := FreeTypeFontProvider readSlotNamed: #current.
	self assert: provider notNil.
	^ provider