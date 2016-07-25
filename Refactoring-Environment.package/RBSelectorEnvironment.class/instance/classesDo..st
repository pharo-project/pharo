accessing-classes
classesDo: aBlock 
	classSelectors keysDo: [ :each | 
		| class |
		class := self systemDictionary at: each ifAbsent: [ nil ].
		(class notNil and: [ environment includesClass: class ])
			ifTrue: [ aBlock value: class ] ].
	metaClassSelectors keysDo: [ :each | 
		| class |
		class := self systemDictionary at: each ifAbsent: [ nil ].
		(class notNil and: [ environment includesClass: class class ])
			ifTrue: [ aBlock value: class class ] ]