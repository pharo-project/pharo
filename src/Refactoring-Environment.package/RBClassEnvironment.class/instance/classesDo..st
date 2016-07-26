accessing-classes
classesDo: aBlock 
	classes do: [ :each | 
		| class |
		class := self systemDictionary at: each ifAbsent: [ nil ].
		(class notNil and: [ environment includesClass: class ])
			ifTrue: [ aBlock value: class ] ].
		metaClasses do: [ :each | 
		| class |
		class := self systemDictionary at: each ifAbsent: [ nil ].
		(class notNil and: [ environment includesClass: class theMetaClass ])
			ifTrue: [ aBlock value: class theMetaClass ] ]