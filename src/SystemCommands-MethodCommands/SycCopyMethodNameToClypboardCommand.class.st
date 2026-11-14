"
Copy selected methods into Clipboard as class>>selector
"
Class {
	#name : 'SycCopyMethodNameToClypboardCommand',
	#superclass : 'SycMethodCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'execution' }
SycCopyMethodNameToClypboardCommand >> executeRefactoring [
	| text |
	text := (methods collect: [ :each | each displayString ]) joinUsing: String cr.
	Clipboard clipboardText: text.
	self inform: 'Copied methods:', String cr, text
]

{ #category : 'accessing' }
SycCopyMethodNameToClypboardCommand >> iconName [
	^#smallCopy
]

{ #category : 'accessing' }
SycCopyMethodNameToClypboardCommand >> name [
	^'Copy method name(s) to Clipboard'
]
