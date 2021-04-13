Extension { #name : #BorderStyle }

{ #category : #'*Morphic-Base' }
BorderStyle class >> complexAltFramed [
	^ComplexBorder style: #complexAltFramed
]

{ #category : #'*Morphic-Base' }
BorderStyle class >> complexAltInset [
	^ComplexBorder style: #complexAltInset
]

{ #category : #'*Morphic-Base' }
BorderStyle class >> complexAltRaised [
	^ComplexBorder style: #complexAltRaised
]

{ #category : #'*Morphic-Base' }
BorderStyle class >> complexFramed [
	^ComplexBorder style: #complexFramed
]

{ #category : #'*Morphic-Base' }
BorderStyle class >> complexInset [
	^ComplexBorder style: #complexInset
]

{ #category : #'*Morphic-Base' }
BorderStyle class >> complexRaised [
	^ComplexBorder style: #complexRaised
]

{ #category : #'*Morphic-Base' }
BorderStyle class >> inset [
	^InsetBorder new
]

{ #category : #'*Morphic-Base' }
BorderStyle class >> raised [
	^RaisedBorder new
]

{ #category : #'*Morphic-Base' }
BorderStyle class >> simple [
	"Answer a simple border style"

	^ SimpleBorder new
]

{ #category : #'*Morphic-Base' }
BorderStyle class >> width: aNumber color: aColor [
	^SimpleBorder new color: aColor; width: aNumber; yourself
]
