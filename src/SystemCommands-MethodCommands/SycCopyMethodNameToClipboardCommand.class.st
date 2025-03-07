"
Copy selected methods into Clipboard as `class>>selector`
"
Class {
	#name : 'SycCopyMethodNameToClipboardCommand',
	#superclass : 'SycMethodExtraCmCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'executing' }
SycCopyMethodNameToClipboardCommand >> applyCommandResult [

	| text |
	text := (self methods collect: #displayString) joinUsing: String cr.
	Clipboard clipboardText: text.
	self inform: 'Copied methods:' , String cr , text
]

{ #category : 'accessing' }
SycCopyMethodNameToClipboardCommand >> description [

	^ 'Copy selected methods into Clipboard as class>>selector'
]

{ #category : 'accessing' }
SycCopyMethodNameToClipboardCommand >> icon [

	^ self iconNamed: #smallCopy
]

{ #category : 'accessing' }
SycCopyMethodNameToClipboardCommand >> name [

	^ 'Copy method name(s) to Clipboard'
]
