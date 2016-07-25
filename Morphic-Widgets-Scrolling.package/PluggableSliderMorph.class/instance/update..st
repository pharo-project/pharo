protocol
update: aParameter

	aParameter = getEnabledSelector 		ifTrue: [ ^ self updateEnabled ].
	aParameter = getValueSelector 		ifTrue: [ ^ self updateValue ].
	aParameter = getLabelSelector 		ifTrue: [ ^ self updateLabel ].
			
	aParameter isArray ifFalse: [ ^ self ].
	aParameter size == 2 ifFalse: [ ^ self ].
	
	aParameter first = #max: ifTrue: [ self max: aParameter second ].
	aParameter first = #min: ifTrue: [ self min: aParameter second ].
	aParameter first = #quantum: ifTrue: [ self quantum: aParameter second ].
	aParameter first = #scaledValue: ifTrue: [ self scaledValue: aParameter second ].
	aParameter first = #value: ifTrue: [ self value: aParameter second ].