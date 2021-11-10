Extension { #name : #UndefinedObject }

{ #category : #'*Deprecated10' }
UndefinedObject >> typeOfClass [
	self
		deprecated: 'Should use mcType instead'
		on: '08 September 2021'
		in: #Pharo10
		transformWith: '`@rec typeOfClass' -> '`@rec mcType'.
	^ self mcType
]
