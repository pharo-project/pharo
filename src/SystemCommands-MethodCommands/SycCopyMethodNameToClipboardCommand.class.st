"
Copy selected methods into Clipboard as `class>>selector`
"
Class {
	#name : 'SycCopyMethodNameToClipboardCommand',
	#superclass : 'SycMethodExtraCmCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'accessing' }
SycCopyMethodNameToClipboardCommand >> description [

	^ 'Copy selected methods into Clipboard as class>>selector'
]

{ #category : 'execution' }
SycCopyMethodNameToClipboardCommand >> execute [

	| text |
	text := (self methods collect: [ :method | method displayString ]) joinUsing: String newLine.
	Clipboard clipboardText: text.
	self inform: 'Copied methods:' , String newLine , text
]

{ #category : 'accessing' }
SycCopyMethodNameToClipboardCommand >> icon [

	^ self iconNamed: #smallCopy
]

{ #category : 'accessing' }
SycCopyMethodNameToClipboardCommand >> name [

	^ 'Copy method name(s) to Clipboard'
]
