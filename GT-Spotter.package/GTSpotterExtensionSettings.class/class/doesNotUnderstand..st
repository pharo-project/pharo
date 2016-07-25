private
doesNotUnderstand: aMessage
	aMessage arguments isEmpty
		ifTrue: [ ^ (self disabledExtensions includes: aMessage selector) not ].
	aMessage arguments size = 1
		ifTrue: [ 
			^ aMessage arguments first
				ifTrue: [ self disabledExtensions 
						remove: aMessage selector allButLast 
						ifAbsent: [ "ignore all settings that are re-enabled but 
										that were not disabled before
										(it can happen when the property was renamed)" ] ]
				ifFalse: [ self disabledExtensions add: aMessage selector allButLast ] ].
	^ super doesNotUnderstand: aMessage