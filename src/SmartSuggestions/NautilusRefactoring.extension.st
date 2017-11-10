Extension { #name : #NautilusRefactoring }

{ #category : #'*SmartSuggestions' }
NautilusRefactoring >> accessorsClassVarNamed: aVariableName from: aClass [
	self performRefactoringFor: #privateAccessorsClassVarNamed:from: with: aVariableName with: aClass
]

{ #category : #'*SmartSuggestions' }
NautilusRefactoring >> accessorsInstVarNamed: aVariableName from: aClass [
	self performRefactoringFor: #privateAccessorsInstVarNamed:from: with: aVariableName with: aClass

]

{ #category : #'*SmartSuggestions' }
NautilusRefactoring >> privateAccessorsClassVarNamed: aVariableName from:  aClass. [
	^ RBCreateAccessorsForVariableRefactoring 
			model: environment
			variable: aVariableName
			class: aClass
			classVariable: true
]

{ #category : #'*SmartSuggestions' }
NautilusRefactoring >> privateAccessorsInstVarNamed: aVariableName from:  aClass. [
	^ RBCreateAccessorsForVariableRefactoring 
			model: environment
			variable: aVariableName
			class: aClass
			classVariable: false
]
