morphic ui
enterSliceName
	| name |
	name := UIManager default request: 'Please describe the fix' initialAnswer: 'OneSentenceSummary'.
		name isEmptyOrNil ifFalse: [
		name isAllAlphaNumerics ifFalse:[
			UIManager default inform:'Your sentence can only be composed of alphanumerics caracters'.
			name := self enterSliceName]].
	^ name