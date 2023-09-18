Extension { #name : #Form }

{ #category : #'*Athens-Core' }
Form >> asAthensPaintOn: canvas [

	^ canvas surface createFormPaint: self
]
