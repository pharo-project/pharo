running
check: aMethod forCritiquesDo: aCriticBlock

	((aMethod methodClass isMeta or: 
	 [ aMethod selector numArgs == 0 or: 
	 [ self utilityProtocols includes: aMethod protocol ] ]) not and: [
		(self 
			subclassOf: aMethod methodClass
			overrides: aMethod selector) not and: [
				aMethod superMessages isEmpty and: [ aMethod selfMessages isEmpty and: [ 
					aMethod methodClass allInstVarNames,
				   aMethod methodClass allClassVarNames asArray,
				 	#('self' )    noneSatisfy: [ :each | aMethod ast references: each ] ] ] ] ])
	ifTrue: [ aCriticBlock cull: (self critiqueFor: aMethod) ]