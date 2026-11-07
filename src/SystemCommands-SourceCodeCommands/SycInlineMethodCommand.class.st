"
I am a command to inline ""self send"" method directly into sender method
"
Class {
	#name : 'SycInlineMethodCommand',
	#superclass : 'SycSourceCodeRefactoringCommand',
	#category : 'SystemCommands-SourceCodeCommands',
	#package : 'SystemCommands-SourceCodeCommands'
}

{ #category : 'testing' }
SycInlineMethodCommand class >> canBeExecutedInContext: aSourceCodeContext [

	^ (super canBeExecutedInContext: aSourceCodeContext) and: [
		  aSourceCodeContext isMethodSelected not and: [
			  aSourceCodeContext isMessageSelected ] ]
]

{ #category : 'converting' }
SycInlineMethodCommand >> asRefactorings [

	^ { ((RBInlineMethodRefactoring inline: self sourceNode sourceInterval inMethod: self method selector forClass: self method origin)
		   setOption: #shouldNotCreateExtraBindings toUse: [ :ref :aString |
			   (self confirm: ('Do you want to inline the expression ''<1s>'' in the current method?' expandMacrosWith: aString)) not ];
		   yourself) }
]

{ #category : 'execution' }
SycInlineMethodCommand >> executeRefactoring [

	^ ((ReInlineMethodDriver newApplication: self application)
		   scopes: refactoringScopes
		   interval: self sourceNode sourceInterval
		   selector: self method selector
		   class: self method origin) runRefactoring 
]

{ #category : 'accessing' }
SycInlineMethodCommand >> icon [

	^ self iconNamed: #smallRightFlush
]

{ #category : 'accessing' }
SycInlineMethodCommand >> name [

	^ 'Inline method'
]

{ #category : 'accessing' }
SycInlineMethodCommand >> order [

	^ 1.4
]
