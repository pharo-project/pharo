"
Copy selected methods selector into Clipboard as testXXX
"
Class {
	#name : 'SycCopyTestMethodNameToClipboardCommand',
	#superclass : 'SycMethodExtraCmCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'accessing' }
SycCopyTestMethodNameToClipboardCommand >> description [
	^ 'Copy selected methods selector into Clipboard as testXXX'
]

{ #category : 'execution' }
SycCopyTestMethodNameToClipboardCommand >> execute [

	| text |
	text := (self methods collect: [ :method | method selector asTestSelector ]) joinUsing: String newLine.
	Clipboard clipboardText: text.
	self inform: 'Copied methods:' , String newLine , text
]

{ #category : 'accessing' }
SycCopyTestMethodNameToClipboardCommand >> icon [

	^ self iconNamed: #smallCopy
]

{ #category : 'accessing' }
SycCopyTestMethodNameToClipboardCommand >> name [

	^ 'Copy test method selector(s) to Clipboard'
]
