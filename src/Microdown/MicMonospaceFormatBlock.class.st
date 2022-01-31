"
I represent a monospaced text section. I'm delimited using `\`` as in `\`monospace\`` to obtain `monospace`.
"
Class {
	#name : #MicMonospaceFormatBlock,
	#superclass : #MicFormatBlock,
	#category : #'Microdown-ModelInline'
}

{ #category : #testing }
MicMonospaceFormatBlock class >> cleanSubstring [
	"does nothing on purpose"
]

{ #category : #testing }
MicMonospaceFormatBlock class >> isEvaluated [
	^ false
]

{ #category : #visiting }
MicMonospaceFormatBlock >> accept: aVisitor [
	^ aVisitor visitMonospace: self
]

{ #category : #internal }
MicMonospaceFormatBlock >> closingDelimiter [

 	^ MicMonospaceDelimiter markup
]

{ #category : #accessing }
MicMonospaceFormatBlock >> kind [
	^ #monospace
]

{ #category : #internal }
MicMonospaceFormatBlock >> openingDelimiter [

 	^ MicMonospaceDelimiter markup
]
