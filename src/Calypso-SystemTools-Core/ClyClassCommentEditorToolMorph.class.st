"
I am an editor of class comment
"
Class {
	#name : #ClyClassCommentEditorToolMorph,
	#superclass : #ClyClassEditorToolMorph,
	#category : #'Calypso-SystemTools-Core-Editors-Classes'
}

{ #category : #activation }
ClyClassCommentEditorToolMorph class >> classCommentTabActivation [
	<classAnnotation>
	
	^ClyTabActivationStrategy for: ClyClassComment asCalypsoItemContext
]

{ #category : #accessing }
ClyClassCommentEditorToolMorph class >> tabOrder [
	^ super tabOrder - 1
]

{ #category : #operations }
ClyClassCommentEditorToolMorph >> applyChanges [

	self pendingText asString = self editingText asString ifFalse: [
		editingClass comment: self pendingText stamp: Author changeStamp].

	^true
]

{ #category : #initialization }
ClyClassCommentEditorToolMorph >> defaultIconName [
	^(context lastSelectedItem isMarkedWith: ClyNoCommentTag)
		ifTrue: [ #uncommentedClass ]
		ifFalse: [ #commentsIcon]
]

{ #category : #initialization }
ClyClassCommentEditorToolMorph >> defaultTitle [
	^'Comment'
]

{ #category : #accessing }
ClyClassCommentEditorToolMorph >> editingText [
	^editingClass comment
]

{ #category : #printing }
ClyClassCommentEditorToolMorph >> printContext [
	^super printContext , ' / ', editingClass name
]

{ #category : #initialization }
ClyClassCommentEditorToolMorph >> setUpDefaultIcon [
	"Here we duplicate logic of #defaultIconName to directly ask class about comment.
	This method is called during background tab building. 
	So it does not really affect performance which is critical for remote scenario"
	
	| iconName |
	iconName := editingClass hasComment ifTrue: [ #commentsIcon ] ifFalse: [ #uncommentedClass ].
	containerTab icon: (self iconNamed: iconName)
]

{ #category : #testing }
ClyClassCommentEditorToolMorph >> wantsTextStyling [
	^false
]
