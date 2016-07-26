visitor
visitRefactoring: aRefactoring 
	^ self displayOperation: (String streamContents: [ :aStream | aRefactoring summaryOn: aStream ])