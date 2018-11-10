Class {
	#name : #CmdContextMenuCommandActivation,
	#superclass : #CmdContextMenuActivation,
	#category : #'Commander-Activators-ContextMenu'
}

{ #category : #deprecation }
CmdContextMenuCommandActivation class >> buildContextMenuFor: aMorph inContext: aToolContext [

	^CmdContextMenuActivation buildContextMenuFor: aMorph inContext: aToolContext
]

{ #category : #deprecation }
CmdContextMenuCommandActivation class >> deprecationRefactorings [

	^ { RBApplyClassDeprecationRefactoring rename: self to: #CmdContextMenuActivation}

]

{ #category : #deprecation }
CmdContextMenuCommandActivation class >> isDeprecated [
	^true
]

{ #category : #deprecation }
CmdContextMenuCommandActivation class >> new [
	^CmdContextMenuActivation new
]
