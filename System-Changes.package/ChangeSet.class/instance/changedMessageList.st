method changes
changedMessageList
	"Used by a message set browser to access the list view information."

	| messageList |
	messageList := OrderedCollection new.
	changeRecords associationsDo: [:clAssoc | | className classIsMeta |
		className := clAssoc key asSymbol.
		classIsMeta := (className findTokens: ' ') size > 1.

		(clAssoc value allChangeTypes includes: #comment) ifTrue:
			[messageList add:
				(RGCommentDefinition new
					parentName: className) asActive ].

		clAssoc value methodChangeTypes associationsDo: [:mAssoc |
			(#(remove addedThenRemoved) includes: mAssoc value) ifFalse:
				[messageList add:
					((RGMethodDefinition named: mAssoc key)
						parentName: className;
						isMetaSide: classIsMeta) asActive ]]].
	^ messageList asArray sort