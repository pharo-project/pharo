Class {
	#name : #MicFormatBlock,
	#superclass : #MicInlineElement,
	#category : #'Microdown-ModelInline'
}

{ #category : #visiting }
MicFormatBlock >> closeMe [
	
	| text |
	
	children ifEmpty: [
			text :=  (MicTextBlock
							from: start
							to: end
							withSubstring: substring
							withChildren: Array empty).
			self children: { text } ]
]

{ #category : #accessing }
MicFormatBlock >> text [
	self flag: #fixMe. 
	"we should clarify what is text! to me it makes no sense."
 	^ String streamContents: [:st | self wrappedElements do: [ :each | st nextPutAll: each text ]]
]

{ #category : #accessing }
MicFormatBlock >> wrappedElements [
	^ children
]
