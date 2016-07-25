changes
defineClass: aString 
	| change newClass newClassName |
	change := changes defineClass: aString.
	newClassName := change changeClassName.
	newClass := self classNamed: newClassName.
	newClass isNil ifTrue: 
		[ | newMetaclass |
		removedClasses
			remove: newClassName
				ifAbsent: [  ];
			remove: newClassName , ' class'
				ifAbsent: [  ].
		newClass := RBClass named: newClassName.
		newMetaclass := RBMetaclass named: newClassName.
		newClass model: self.
		newMetaclass model: self.
		newClasses 
			at: newClassName
			put: (Array 
					with: newClass
					with: newMetaclass) ].
	newClass superclass: (self classNamed: change superclassName).
	newClass superclass isNil 
		ifTrue: 
			[ self rootClasses add: newClass.
			newClass theMetaClass superclass: (self classFor: Object class superclass) ]
		ifFalse: [ newClass theMetaClass superclass: newClass superclass theMetaClass ].
	newClass instanceVariableNames: change instanceVariableNames.
	newClass classVariableNames: change classVariableNames.
	newClass poolDictionaryNames: change sharedPoolNames.
	newClass category: change category.
	^ change