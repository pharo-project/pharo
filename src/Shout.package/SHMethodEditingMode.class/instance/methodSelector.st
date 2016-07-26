view
methodSelector
	| entryCompletion applicantsBlock |
	applicantsBlock := [self selectedClass 
						ifNil: [#()] 
						ifNotNil: [:clsn | self classToUse selectors asArray sort ]].
	entryCompletion := EntryCompletion new
				dataSourceBlock: [:currText | applicantsBlock value];
				filterBlock: [:currApplicant :currText | currText notEmpty
						and: [currApplicant asUppercase includesSubstring: currText asString asUppercase]].
	^ (UIManager default newAutoAcceptTextEntryFor: self
			get: #selector
			set: #selector:
			class: String 
			getEnabled: #haveClassName
			help: 'Enter a selector' translated
			entryCompletion: entryCompletion) 
				acceptOnCR: false;
				ghostText: 'Selector';
				withDropListButton;
				yourself.
