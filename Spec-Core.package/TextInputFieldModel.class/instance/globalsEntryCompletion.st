private
globalsEntryCompletion

	| applicants |
	applicants := (Array
				streamContents: [:strm | Symbol
						allSymbolTablesDo: [:each | (each notEmpty
									and: [each first canBeGlobalVarInitial
											and: [Smalltalk globals includesKey: each]])
								ifTrue: [strm nextPut: each]]]) sort.

	^ EntryCompletion new
				dataSourceBlock: [:currText | applicants];
				filterBlock: [:currApplicant :currText | currText size > 3
						and: [currApplicant asUppercase includesSubstring: currText asString asUppercase]].