Extension { #name : #MicroDownParser }

{ #category : #'*Microdown-RichTextComposer' }
MicroDownParser class >> convertToRichText: aString [
	^ MicRichTextComposer new visit: (self new parse: aString)
]
