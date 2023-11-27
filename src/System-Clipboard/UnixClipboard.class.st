"
The Unix vm does not do line-ending conversion, thus this subclass which specifies we convert CR's to LF's when pasting to it
"
Class {
	#name : 'UnixClipboard',
	#superclass : 'Clipboard',
	#category : 'System-Clipboard',
	#package : 'System-Clipboard'
}

{ #category : 'accessing' }
UnixClipboard >> clipboardText: text [

	| string bytes |
	string := text asString.
	self noteRecentClipping: text asText copyWithoutExternalReferences.
	contents := text asText copyWithoutExternalReferences.
	string := string replaceAll: Character cr with: Character lf.
	bytes := string utf8Encoded.
	self primitiveClipboardText: bytes
]