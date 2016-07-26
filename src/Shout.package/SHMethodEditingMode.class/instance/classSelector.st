view
classSelector
	| applicantsBlock entryCompletion |
	applicantsBlock := [(Array
				streamContents: [:strm | Symbol
						allSymbolTablesDo: [:each | (each notEmpty
									and: [each first canBeGlobalVarInitial
											and: [Smalltalk globals includesKey: each]])
								ifTrue: [strm nextPut: each]]]) sort].
	entryCompletion := EntryCompletion new
				dataSourceBlock: [:currText | applicantsBlock value ];
				filterBlock: [:currApplicant :currText | currText notEmpty
						and: [currApplicant asUppercase includesSubstring: currText asString asUppercase]].
	^ (UIManager default newAutoAcceptTextEntryFor: self
			get: #className
			set: #className:
			class: String 
			getEnabled: nil
			help: 'Enter a class name' translated
			entryCompletion: entryCompletion)
				acceptOnCR: false;
				ghostText: 'Class name';
				withDropListButton;
				yourself.
