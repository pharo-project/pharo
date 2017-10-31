examples
exampleTextEntryWithCompletionDialog
	"self exampleTextEntryWithCompletionDialog"
	<script>
	| applicants entryCompletion |
	applicants := (Array
				streamContents: [:strm | Symbol
						allSymbolTablesDo: [:each | (each notEmpty
									and: [each first canBeGlobalVarInitial
											and: [Smalltalk globals includesKey: each]])
								ifTrue: [strm nextPut: each]]]) sort.
	entryCompletion := EntryCompletion new
				dataSourceBlock: [:currText | applicants];		
				filterBlock: [:currApplicant :currText | currText size > 3
						and: [currApplicant asUppercase includesSubstring: currText asString asUppercase]].
	UIManager default
		request: 'Enter a class name fragment (at least 4 characters)'
		initialAnswer: ''
		title: 'Text entry with completion example: simple find a class'
		entryCompletion: entryCompletion