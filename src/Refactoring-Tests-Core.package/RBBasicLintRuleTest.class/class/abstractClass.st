possible bugs
abstractClass
	| detector subclassResponsibilitySymbol |
	detector := self new.
	detector name: 'References an abstract class'.
	detector resultClass: RBClassEnvironment.
	subclassResponsibilitySymbol := 'subclassResponsibility' asSymbol.
	detector classBlock: 
			[:context :result | 
			(context selectedClass whichSelectorsReferTo: subclassResponsibilitySymbol)
				ifNotEmpty: 
						[(context whichSelectorsReferTo: subclassResponsibilitySymbol) ifNotEmpty:  [result addClass: context selectedClass]]].
	^detector