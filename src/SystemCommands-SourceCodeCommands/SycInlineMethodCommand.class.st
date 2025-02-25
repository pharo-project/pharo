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

	^ { ((RBInlineMethodRefactoring
		    inline: sourceNode sourceInterval
		    inMethod: method selector
		    forClass: method origin)
		   setOption: #shouldNotCreateExtraBindings toUse: [ :ref :aString |
			   (self 	confirm: 	('Do you want to inline the expression ''<1s>'' in the current method?' expandMacrosWith: aString)) not ];
		   yourself) }
]

{ #category : 'accessing' }
SycInlineMethodCommand >> defaultMenuIconName [
	^ #smallRightFlush
]

{ #category : 'accessing' }
SycInlineMethodCommand >> defaultMenuItemName [
	^ 'Inline method'
]
