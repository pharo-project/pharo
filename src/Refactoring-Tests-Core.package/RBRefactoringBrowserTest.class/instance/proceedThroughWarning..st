private
proceedThroughWarning: aBlock 
	aBlock on: RBRefactoringWarning do: [ :ex | ex resume ]