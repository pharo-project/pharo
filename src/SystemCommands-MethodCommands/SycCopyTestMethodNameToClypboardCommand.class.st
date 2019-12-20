"
Copy selected methods selector into Clipboard as testOriginalSelector
"
Class {
	#name : #SycCopyTestMethodNameToClypboardCommand,
	#superclass : #SycMethodCommand,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #accessing }
SycCopyTestMethodNameToClypboardCommand >> defaultMenuIconName [
	^#smallCopy
]

{ #category : #accessing }
SycCopyTestMethodNameToClypboardCommand >> defaultMenuItemName [
	^'Copy test method selector(s) to Clipboard'
]

{ #category : #accessing }
SycCopyTestMethodNameToClypboardCommand >> description [
	^'Copy selected methods selector into Clipboard as testXXX'
]

{ #category : #execution }
SycCopyTestMethodNameToClypboardCommand >> execute [
	| text |
	text := (methods collect: [ :each | each selector asTestSelector ]) joinUsing: String cr.
	
	Clipboard clipboardText: text.
	
	self inform: 'Copied methods:', String cr, text
]
