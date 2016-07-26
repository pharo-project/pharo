private
allClassesDo: aBlock 
	| classes instVarBlock |
	classes := Set new.
	instVarBlock := 
			[:each | 
			| class |
			class := self classForName: each.
			classes addAll: class withAllSubclasses].
	instanceVariables keysDo: instVarBlock.
	instanceVariableReaders keysDo: instVarBlock.
	instanceVariableWriters keysDo: instVarBlock.
	classVariables keysDo: 
			[:each | 
			| class |
			class := self classForName: each.
			class notNil 
				ifTrue: 
					[classes
						addAll: class withAllSubclasses;
						addAll: class class withAllSubclasses]].
	classes do: aBlock