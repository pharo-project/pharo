*Hermes-Ring
bindingOf: aName
	
	^ self 
		at: aName
		ifPresent: [ :value | GlobalVariable key: aName value: value ]
		ifAbsent: [ nil ]