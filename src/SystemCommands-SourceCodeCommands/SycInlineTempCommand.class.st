"
I am a command to inline selected temp variable (represented by source node)
"
Class {
	#name : 'SycInlineTempCommand',
	#superclass : 'SycSourceCodeRefactoringCommand',
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'testing' }
SycInlineTempCommand class >> canBeExecutedInContext: aSourceCodeContext [
	^ (super canBeExecutedInContext: aSourceCodeContext) and: [ aSourceCodeContext isTempVariableSelected]
]

{ #category : 'converting' }
SycInlineTempCommand >> asRefactorings [ 
	| assignment |
	assignment := self sourceNode methodNode assignmentNodes
		detect: [:each | each variable = self sourceNode ].

	^ { ReInlineTemporaryRefactoring
		inline: assignment sourceInterval
		from: self method selector
		in: self method origin }

]

{ #category : 'execution' }
SycInlineTempCommand >> executeRefactoring [

	| assignment |
	assignment := self sourceNode methodNode assignmentNodes detect: [ :each | each variable = self sourceNode ].

	^ { ((ReInlineTemporaryDriver newApplication: self application)
		   scopes: refactoringScopes
		   interval: assignment sourceInterval
		   selector: self method selector
		   class: self method origin) runRefactoring }
]

{ #category : 'accessing' }
SycInlineTempCommand >> icon [

	^ self iconNamed: #smallRightFlush
]

{ #category : 'accessing' }
SycInlineTempCommand >> name [

	^ 'Inline temp'
]

{ #category : 'accessing' }
SycInlineTempCommand >> order [

	^ 1.4
]
