Class {
	#name : #SycCopyMethodNameToClypboardCommand,
	#superclass : #SycMethodCommand,
	#category : #'SystemCommands-MethodCommands'
}

{ #category : #accessing }
SycCopyMethodNameToClypboardCommand >> defaultMenuIconName [
	^#smallCopy
]

{ #category : #accessing }
SycCopyMethodNameToClypboardCommand >> defaultMenuItemName [
	^'Copy name(s) to Clipboard'
]

{ #category : #accessing }
SycCopyMethodNameToClypboardCommand >> description [
	^'Copy selected methods into Clipboard as class>>selector'
]

{ #category : #execution }
SycCopyMethodNameToClypboardCommand >> execute [
	| text |
	text := (methods collect: [ :each | each displayString ]) joinUsing: String cr.
	
	Clipboard clipboardText: text.
	
	self inform: 'Copied methods:', String cr, text
]
