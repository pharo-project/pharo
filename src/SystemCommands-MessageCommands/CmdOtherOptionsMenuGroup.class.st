Class {
	#name : #CmdOtherOptionsMenuGroup,
	#superclass : #CmdMenuGroup,
	#category : #'SystemCommands-MessageCommands-Groups'
}

{ #category : #parents }
CmdOtherOptionsMenuGroup class >> parentGroup [
	^CmdRefactorMenuGroup 
]

{ #category : #testing }
CmdOtherOptionsMenuGroup >> isInlined [
	^true
]

{ #category : #accessing }
CmdOtherOptionsMenuGroup >> order [
	^ 20
]
