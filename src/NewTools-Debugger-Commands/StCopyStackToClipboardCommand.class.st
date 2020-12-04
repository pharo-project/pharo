"
Requests the debugger to copy its stack on the Clipboard
"
Class {
	#name : #StCopyStackToClipboardCommand,
	#superclass : #StDebuggerCommand,
	#category : #'NewTools-Debugger-Commands'
}

{ #category : #initialization }
StCopyStackToClipboardCommand class >> defaultDescription [
	^ 'Copy a short debugging stack to the clipboard.'
]

{ #category : #initialization }
StCopyStackToClipboardCommand class >> defaultIconName [

	^ #debuggerCopyStack
]

{ #category : #initialization }
StCopyStackToClipboardCommand class >> defaultName [
	^ 'Copy Stack to Clipboard'
]

{ #category : #initialization }
StCopyStackToClipboardCommand class >> defaultShortcut [
	^ nil
]

{ #category : #testing }
StCopyStackToClipboardCommand >> appliesTo: aDebugger [
	^ true
]

{ #category : #hooks }
StCopyStackToClipboardCommand >> execute [
	self debugger copyStackToClipboard
]
