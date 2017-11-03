"
A CopyToClipboardDebugAction copies a short debugging stack to the clipboard.

"
Class {
	#name : #CopyToClipboardDebugAction,
	#superclass : #DebugAction,
	#category : #DebuggerActions
}

{ #category : #registration }
CopyToClipboardDebugAction class >> actionType [
	<contextMenuDebuggingAction>
]

{ #category : #accessing }
CopyToClipboardDebugAction >> defaultLabel [

	^ 'Copy Stack to Clipboard'
]

{ #category : #accessing }
CopyToClipboardDebugAction >> defaultOrder [

	^ 120
]

{ #category : #actions }
CopyToClipboardDebugAction >> executeAction [

	Clipboard clipboardText: (String streamContents: [ :s| 
		self debugger interruptedContext shortDebugStackOn: s ])
]

{ #category : #accessing }
CopyToClipboardDebugAction >> id [

	^ #copyToClipboard
]
