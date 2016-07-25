test unique identifier
testUniqueIdentifierName

	| idToRules |

	idToRules := Dictionary new.
	RBLintRule allSubclasses do: [ :ruleClass | 
		(idToRules at: ruleClass uniqueIdentifierName ifAbsentPut: [ Set new ]) add: ruleClass ].
	self assert: (idToRules noneSatisfy: [ :ruleClasses | ruleClasses size > 1 ]).
