*Tool-Base
classFromPattern: pattern withCaption: aCaption
	"If there is a class whose name exactly given by pattern, return it.
	If there is only one class in the system whose name matches pattern, return it.
	Otherwise, put up a menu offering the names of all classes that match pattern, and return the class chosen, else nil if nothing chosen.
	This method ignores tab, space, & cr characters in the pattern"

	| toMatch potentialClassNames classNames exactMatch index |
	(toMatch := pattern
		copyWithoutAll:
			{(Character space).
			(Character cr).
			(Character tab)}) isEmpty
		ifTrue: [ ^ nil ].
	Symbol
		hasInterned: toMatch
		ifTrue: [ :patternSymbol | 
			self environment
				at: patternSymbol
				ifPresent: [ :maybeClass | 
					^ maybeClass isClassOrTrait 
						ifTrue: [ maybeClass ]
						ifFalse: [ maybeClass class ]
					]].
	toMatch := (toMatch copyWithout: $.) asLowercase.
	potentialClassNames := (self environment classNames , self environment traitNames) asOrderedCollection.
	classNames := pattern last = $.
		ifTrue: [ potentialClassNames select: [ :nm | nm asLowercase = toMatch ] ]
		ifFalse: [ potentialClassNames select: [ :n | n includesSubstring: toMatch caseSensitive: false ] ].
	classNames isEmpty
		ifTrue: [ ^ nil ].
	exactMatch := classNames detect: [ :each | each asLowercase = toMatch ] ifNone: [ nil ].
	index := classNames size = 1
		ifTrue: [ 1 ]
		ifFalse: [ 
			exactMatch
				ifNil: [ UIManager default chooseFrom: classNames lines: #() title: aCaption ]
				ifNotNil: [ 
					classNames addFirst: exactMatch.
					UIManager default chooseFrom: classNames lines: #(1) title: aCaption ] ].
	index = 0
		ifTrue: [ ^ nil ].
	^ self environment at: (classNames at: index) asSymbol	"
	self default classFromPattern: 'znak' withCaption: ''
	self default classFromPattern: 'orph' withCaption: ''
	self default classFromPattern: 'TCompil' withCaption: ''
"