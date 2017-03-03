installing
createClass
	| superClass |
	"Ignore Context definition because of troubles with class migration on bootstrapped image. Temporary solution."
	name = #Context
		ifTrue: [ Context comment = comment 
						ifFalse: [ Context comment: comment stamp: commentStamp ].
					^ self ].
	superClass := superclassName == #nil
		ifFalse: [ Smalltalk globals at: superclassName ].
	^ [ PharoClassInstaller
		make: [ :builder | 
			builder
				superclass: superClass;
				name: name;
				layoutClass: (ObjectLayout layoutForType: type);
				slots: self instanceVariables;
				sharedVariables: self classVariables;
				sharedPools: self sharedPoolsString;
				classSlots: self classInstanceVariables;
				traitComposition: self traitCompositionCompiled;
				classTraitComposition: self classTraitCompositionCompiled;
				comment: comment stamp: commentStamp;
				category: category;
				environment: superClass environment ] ]
		on: Warning , DuplicatedVariableError
		do: [ :ex | ex resume ]