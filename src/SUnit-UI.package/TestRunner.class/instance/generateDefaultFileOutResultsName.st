actions
generateDefaultFileOutResultsName
	
	| filename |
	filename := String
		streamContents: [ :st | 
			| printedDate |
			st nextPutAll: 'tests_'.
			st nextPutAll: SystemVersion current version.
			st nextPutAll: '_'.
			result timeStamp printYMDOn: st.
			st nextPutAll: '_'.
			result timeStamp printHMSOn: st.
			st nextPutAll: '.txt' ].
	^ filename copyReplaceAll: ':' with: '-'.