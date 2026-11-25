Extension { #name : 'BorderStyle' }

{ #category : '*Morphic-Widgets' }
BorderStyle class >> complexAltFramed [
	^ComplexBorderStyle style: #complexAltFramed
]

{ #category : '*Morphic-Widgets' }
BorderStyle class >> complexAltInset [
	^ComplexBorderStyle style: #complexAltInset
]

{ #category : '*Morphic-Widgets' }
BorderStyle class >> complexAltRaised [
	^ComplexBorderStyle style: #complexAltRaised
]

{ #category : '*Morphic-Widgets' }
BorderStyle class >> complexFramed [
	^ComplexBorderStyle style: #complexFramed
]

{ #category : '*Morphic-Widgets' }
BorderStyle class >> complexInset [
	^ComplexBorderStyle style: #complexInset
]

{ #category : '*Morphic-Widgets' }
BorderStyle class >> complexRaised [
	^ComplexBorderStyle style: #complexRaised
]

{ #category : '*Morphic-Widgets' }
BorderStyle class >> raised [
	^RaisedBorderStyle new
]

{ #category : '*Morphic-Widgets' }
BorderStyle class >> simple [
	"Answer a simple border style"

	^ SimpleBorderStyle new
]
