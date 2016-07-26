user-interface
newFindTextEntryMorph
	| entryCompletion pt |
	entryCompletion := EntryCompletion new
				dataSourceBlock: [:currText | self prevFinds ];
				filterBlock: [:currApplicant :currText | currText size = 0
						or: [currApplicant asUppercase includesSubstring: currText asString asUppercase]].
	pt := (self newAutoAcceptTextEntryFor: self
			get: #findText
			set: #findText:
			class: String 
			getEnabled: nil
			help: 'Enter the text to find' translated
			entryCompletion: entryCompletion) 
		acceptOnCR: false;
		ghostText: 'Text to find';
		withDropListButton;
		crAction: [:t | self find];
		yourself.
	pt textMorph
		autoFit: true;
		wrapFlag: false;
		margins: (2@1 corner: 2@1).
	findTextField := pt.
	^ pt
