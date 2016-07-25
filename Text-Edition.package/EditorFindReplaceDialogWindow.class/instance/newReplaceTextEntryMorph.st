user-interface
newReplaceTextEntryMorph
	| entryCompletion pt |
	entryCompletion := EntryCompletion new
				dataSourceBlock: [:currText | self prevReplacements ];
				filterBlock: [:currApplicant :currText | currText size = 0
						or: [currApplicant asUppercase includesSubstring: currText asString asUppercase]].

	pt := (self
		newAutoAcceptTextEntryFor: self
		get: #replaceText
		set: #replaceText:
		class: String
		getEnabled: nil
		help: 'Enter the replacement text' translated
		entryCompletion: entryCompletion)
			acceptOnCR: true;
			crAction: [:t | self replace];
			withDropListButton;
			yourself.
	pt textMorph
		autoFit: true;
		wrapFlag: false;
		margins: (2@1 corner: 2@1).
	^ pt
