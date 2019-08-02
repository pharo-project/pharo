"
I am a command to push down given methods
"
Class {
	#name : #SycPushDownMethodCommand,
	#superclass : #SycMethodCommand,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #activation }
SycPushDownMethodCommand class >> methodContextMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation 
		byItemOf: ClyMoreRefactorsMenuGroup 
		order: 20 
		for: ClyMethod asCalypsoItemContext
]

{ #category : #activation }
SycPushDownMethodCommand class >> methodShortcutActivation [
	<classAnnotation>
	
	^CmdShortcutActivation by: $p meta, $d meta for: ClyMethod asCalypsoItemContext
]

{ #category : #execution }
SycPushDownMethodCommand >> asRefactorings [

	^methods collect: [:method | 
		RBPushDownMethodRefactoring pushDown: {method selector} from: method origin]
]

{ #category : #accessing }
SycPushDownMethodCommand >> defaultMenuIconName [ 
	^ #down
]

{ #category : #accessing }
SycPushDownMethodCommand >> defaultMenuItemName [
	^'Push down'
]

{ #category : #execution }
SycPushDownMethodCommand >> execute [

	self executeRefactorings
]

{ #category : #testing }
SycPushDownMethodCommand >> isComplexRefactoring [
	^true
]
