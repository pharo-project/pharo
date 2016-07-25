bugs
subclassResponsibilityNotDefined
	| detector subclassResponsibilitySymbol |
	detector := self new.
	subclassResponsibilitySymbol := 'subclassResponsibility' asSymbol.
	detector name: 'Subclass responsibility not defined'.
	detector classBlock: 
			[:context :result | 
			(context selectedClass whichSelectorsReferTo: subclassResponsibilitySymbol)
				do: 
					[:each | 
					(context selectedClass withAllSubclasses detect: 
							[:class | 
							class subclasses isEmpty
								and: [(class whichClassIncludesSelector: each) == context selectedClass]]
						ifNone: [nil]) notNil
						ifTrue: [result addClass: context selectedClass selector: each]]].
	^detector