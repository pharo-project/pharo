*Shift-Changes-Ficus
compareTo: anotherDefinition
	| result | 
	result := OrderedCollection new.
	
	self changeValidations do:[:e | e checkChangeOf: self with: anotherDefinition puttingResultIn: result].
	
	^ result 