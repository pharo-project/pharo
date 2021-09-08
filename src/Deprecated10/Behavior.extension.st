Extension { #name : #Behavior }

{ #category : #'*Deprecated10' }
Behavior >> typeOfClass [

	self
		deprecated: 'Should use mcType instead'
		on: '08 September 2021'
		in: #Pharo10
		transformWith: '`@rec typeOfClass' -> '`@rec mcType'.
	^ self mcType
]
