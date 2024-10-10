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

{ #category : 'executing' }
SycCopyTestMethodNameToClipboardCommand >> execute [
	| text |

	text := (self methods collect: [ :each | each selector asTestSelector ]) joinUsing: String cr.
	Clipboard clipboardText: text.
	self inform: 'Copied methods:' , String cr , text
]

{ #category : 'accessing' }
SycCopyTestMethodNameToClipboardCommand >> icon [

	^ self iconNamed: #smallCopy
]

{ #category : 'accessing' }
SycCopyTestMethodNameToClipboardCommand >> name [

	^ 'Copy test method selector(s) to Clipboard'
]
