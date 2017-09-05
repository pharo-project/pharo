activation
initializeProtocolsFrom: aFileName

	(File named: aFileName) readStreamDo: [:binaryStream | | stream |
		stream := ZnCharacterReadStream on: binaryStream encoding: 'utf8'.
		[stream atEnd] whileFalse: [ | line items selector class |
			line := stream upTo: Character cr.
			items := line findTokens: String tab.
			class := (items second = 'true') 
				ifTrue: [(Smalltalk classOrTraitNamed: items first) theMetaClass]
				ifFalse: [Smalltalk classOrTraitNamed: items first].
			selector := items third asSymbol.
			(class localSelectors includes: selector)
				ifTrue: [class organization classify: selector under: items fourth]]].