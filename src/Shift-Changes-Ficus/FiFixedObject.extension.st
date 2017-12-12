Extension { #name : #FiFixedObject }

{ #category : #'*Shift-Changes-Ficus' }
FiFixedObject >> changeValidations [
	^ #().
]

{ #category : #'*Shift-Changes-Ficus' }
FiFixedObject >> compareTo: anotherDefinition [
	| result | 
	result := OrderedCollection new.
	
	self changeValidations do:[:e | e checkChangeOf: self with: anotherDefinition puttingResultIn: result].
	
	^ result 
]
