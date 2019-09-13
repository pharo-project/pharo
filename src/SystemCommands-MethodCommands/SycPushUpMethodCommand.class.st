"
I am a command to push up given methods
"
Class {
	#name : #SycPushUpMethodCommand,
	#superclass : #SycMethodCommand,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #activation }
SycPushUpMethodCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation 
		byItemOf: ClyMoreRefactorsMenuGroup 
		order: 10 
		for: ClyMethod asCalypsoItemContext
]

{ #category : #activation }
SycPushUpMethodCommand class >> methodShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $p meta, $u meta for: ClyMethod asCalypsoItemContext
]

{ #category : #execution }
SycPushUpMethodCommand >> asRefactorings [
	|dialog refactoring|
	refactoring :=  RBPullUpMethodRefactoring 
		pullUp: methods  
		from: methods first origin.
	refactoring setOption: #superClass toUse: [ :ref |
		dialog := RBPushUpPreview on: ref.
		dialog cancelled ifTrue: [ CmdCommandAborted signal ]].
	^ OrderedCollection with: refactoring 
]

{ #category : #accessing }
SycPushUpMethodCommand >> defaultMenuIconName [ 
	^ #up
]

{ #category : #accessing }
SycPushUpMethodCommand >> defaultMenuItemName [
	^'Push up'
]

{ #category : #execution }
SycPushUpMethodCommand >> execute [

	self executeRefactorings
]

{ #category : #testing }
SycPushUpMethodCommand >> isComplexRefactoring [
	^true
]
