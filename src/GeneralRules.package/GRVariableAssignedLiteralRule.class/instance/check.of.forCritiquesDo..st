running-helpers
check: aSlot of: aClass forCritiquesDo: aCriticBlock
	| theMethod |

	theMethod := nil.

	aClass withAllSubclassesDo: [ :class |
		class methods do: [ :method |
			(method writesField: aSlot index) ifTrue: [ 
				theMethod
					ifNotNil: [ ^ self ]
					ifNil: [theMethod := method ] ] ] ].
	
	theMethod ifNil: [ ^ self ].
	
	(self matchMethod: theMethod with: aSlot name) ifTrue: [ 
		aCriticBlock cull: (
			self 
				critiqueFor: aClass
				about: aSlot name) ]