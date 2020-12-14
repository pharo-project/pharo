"
The clipboard for MacOS
"
Class {
	#name : #MacOSClipboard,
	#superclass : #ExternalClipboard,
	#category : #'System-Clipboard'
}

{ #category : #private }
MacOSClipboard >> addUF8StringClipboardData: aString [
	| ba  |

	self clearClipboard.
	ba := aString utf8Encoded.
	self addClipboardData: ba dataFormat: 'public.utf8-plain-text'

]
