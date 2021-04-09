Extension { #name : #BorderStyle }

{ #category : #'*Morphic-Base' }
BorderStyle class >> complexAltFramed [
	^ComplexBorderStyle style: #complexAltFramed
]

{ #category : #'*Morphic-Base' }
BorderStyle class >> complexAltInset [
	^ComplexBorderStyle style: #complexAltInset
]

{ #category : #'*Morphic-Base' }
BorderStyle class >> complexAltRaised [
	^ComplexBorderStyle style: #complexAltRaised
]

{ #category : #'*Morphic-Base' }
BorderStyle class >> complexFramed [
	^ComplexBorderStyle style: #complexFramed
]

{ #category : #'*Morphic-Base' }
BorderStyle class >> complexInset [
	^ComplexBorderStyle style: #complexInset
]

{ #category : #'*Morphic-Base' }
BorderStyle class >> complexRaised [
	^ComplexBorderStyle style: #complexRaised
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
