"
A ClipboardTest is a test class for testing the behavior of Clipboard
"
Class {
	#name : 'ClipboardTest',
	#superclass : 'TestCase',
	#instVars : [
		'clipboard'
	],
	#category : 'System-Clipboard-Tests-Base',
	#package : 'System-Clipboard-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
ClipboardTest >> setUp [

	super setUp.
	clipboard := Clipboard new
]

{ #category : 'tests' }
ClipboardTest >> testContentsIsText [

 	self assert: clipboard clipboardText isText.

	clipboard clipboardText: 'Someting'.

 	self assert: clipboard clipboardText isText
]

{ #category : 'tests' }
ClipboardTest >> testDoNotRemeberSameTextTwice [

	self deny: clipboard hasRecentClippings.

	clipboard clipboardText: 'Text'.
	clipboard clipboardText: 'AnotherText'.
	clipboard clipboardText: 'Text'.

	self assert: clipboard recentClippings size equals: 2
]

{ #category : 'tests' }
ClipboardTest >> testInitiallyEmpty [

 	self assert: clipboard clipboardText isEmpty.

	self deny: clipboard hasRecentClippings
]

{ #category : 'tests' }
ClipboardTest >> testRecentClippingIsLIFOAndRotating [

	| remembered |
	self deny: clipboard hasRecentClippings.

	1 to: 6 do: [ :i | clipboard clipboardText: 'Entry ' , i printString ].

	self assert: clipboard hasRecentClippings.

	remembered := clipboard recentClippings.

	self assert: remembered size equals: 5.

	"Entry 6 has been last clipped, so it must be first"
	self assert: remembered first asString equals: 'Entry 6'.

	"Entry 1 and 2 had been clipped first, Entry 1 vanished, so Entry 2 must be last"
	self assert: remembered last asString equals: 'Entry 2'
]
