system annoucements
classAdded: aClass

	| rules |

	(rbEnvironment definesClass: aClass) ifFalse: [ ^ self ].

	rules := self allRules.
	checker
		resetResult;
		checkClass: aClass.
		
	rules do: [ :rule |		
		(checker criticsOf: rule) do:	[ :crit |
			cache addCritic: crit forRule: rule ].
		(checker falsePositiveOf: rule) do:	[ :crit |
			cache addFalsePositive: crit forRule: rule ].
		(checker toDoOf: rule) do:	[ :crit |
			cache addToDo: crit forRule: rule ] ].
	
	cache updateBrowser.