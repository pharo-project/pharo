running
checkClass: aClass 

	| explicitRequirementMethods |
	explicitRequirementMethods := Set new.
	aClass isTrait 
		ifTrue: [ ^ self ].
		
	aClass traitComposition allSelectors do: [ :selector | | method |
				method := aClass>>selector.
				 (method isRequired and: [ method isSubclassResponsibility not ]) 
						ifTrue: [ explicitRequirementMethods add: method ]].
	
	^ explicitRequirementMethods do: [ :method |
		aClass withAllSuperclasses 
			detect: [ :superclass | superclass canPerform: method selector ] ifNone: [result addClass: aClass selector: method selector]].