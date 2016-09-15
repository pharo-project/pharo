visitor
visitRefactoring: aRefactoring 

	^  (self displayOperation: 'Refactoring: '), 
		(self displayString: (String streamContents: [ :aStream | 
			aRefactoring summaryOn: aStream ]))