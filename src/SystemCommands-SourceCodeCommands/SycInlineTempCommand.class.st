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
	assignment := sourceNode methodNode assignmentNodes
		detect: [:each | each variable = sourceNode ].

	^ { ReInlineTemporaryRefactoring
		inline: assignment sourceInterval
		from: method selector
		in: method origin }

]

{ #category : 'accessing' }
SycInlineTempCommand >> defaultMenuIconName [
	^ ##smallRightFlush
]

{ #category : 'accessing' }
SycInlineTempCommand >> defaultMenuItemName [
	^'Inline temp'
]

{ #category : 'execution' }
SycInlineTempCommand >> execute [

	| assignment |
	assignment := sourceNode methodNode assignmentNodes detect: [ :each | each variable = sourceNode ].

	^ { ((ReInlineTemporaryDriver newApplication: self application)
		   scopes: refactoringScopes
		   interval: assignment sourceInterval
		   selector: method selector
		   class: method origin) runRefactoring }
]

{ #category : 'execution' }
SycInlineTempCommand >> prepareFullExecutionInContext: aSourceCodeContext [

	super prepareFullExecutionInContext: aSourceCodeContext.
	refactoringScopes := aSourceCodeContext refactoringScopes
]
