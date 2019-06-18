"
I represent kind of reusable additional method group which users can reuse for grouping set of extra rarely used commands.

I am always the last group in the menu which is always separated by line in the context menu 
"
Class {
	#name : #CmdExtraMenuGroup,
	#superclass : #CmdMenuGroup,
	#category : #'Commander-Core'
}

{ #category : #accessing }
CmdExtraMenuGroup >> description [
	^'Rarely used commands'
]

{ #category : #accessing }
CmdExtraMenuGroup >> name [
	^'Extra'
]

{ #category : #accessing }
CmdExtraMenuGroup >> order [
	"Idea to put extra group to the end of menu"
	^SmallInteger maxVal
]
