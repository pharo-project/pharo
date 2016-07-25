accessing-classes
keys
	| keys |
	keys := Set new.
	self systemDictionary keysAndValuesDo: [ :key :value | 
		| class |
		value isBehavior ifTrue: [
			(self includesClass: value) 
				ifTrue: [ keys add: key ] ].
			class := value class.
			(self includesClass: class) 
				ifTrue: [ keys add: key ] ].
	^ keys