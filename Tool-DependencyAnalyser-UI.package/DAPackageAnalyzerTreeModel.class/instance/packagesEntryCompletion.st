accessing
packagesEntryCompletion
	| applicants |
	applicants := (RPackageOrganizer default packages collect: [ :package | package packageName asString ]).

	^ EntryCompletion new
				dataSourceBlock: [:currText | applicants];
				filterBlock: [:currApplicant :currText | currText size > 3
						and: [currApplicant asUppercase includesSubstring: currText asString asUppercase]].