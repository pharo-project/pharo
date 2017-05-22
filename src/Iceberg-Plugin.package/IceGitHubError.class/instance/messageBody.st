accessing
messageBody
	| errors |
	errors := self jsonContents at: #errors ifAbsent: [ ^ '' ].
	^ String streamContents: [ :stream | 
		errors
			do: [ :each | stream << (self errorToString: each) ] 
			separatedBy: [ stream cr ] ].