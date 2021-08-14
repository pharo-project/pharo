"
A ResetWindow is simple UI to reset the currently analysed model.
"
Class {
	#name : #ResetWindow,
	#superclass : #CBResetWindow,
	#category : #'Tool-CriticBrowser-Parts'
}

{ #category : #specs }
ResetWindow class >> isDeprecated [ 
	"We do not need this class anymore. Use CBResetWindow instead" 

	^true
]
