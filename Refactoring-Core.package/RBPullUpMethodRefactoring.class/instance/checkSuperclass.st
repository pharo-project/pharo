preconditions
checkSuperclass
	| overrideSelectors |
	overrideSelectors := selectors 
				select: [:each | class superclass definesMethod: each].
	overrideSelectors := overrideSelectors reject: 
					[:each | 
					| myTree superTree |
					myTree := class parseTreeFor: each.
					superTree := class superclass parseTreeFor: each.
					superTree equalTo: myTree exceptForVariables: #()].
	overrideSelectors isEmpty ifTrue: [^self].
	class superclass isAbstract 
		ifFalse: 
			[self refactoringError: ('Non-abstract class <2p> already defines <1p>' 
						expandMacrosWith: overrideSelectors asArray first
						with: class superclass)].
	overrideSelectors do: [:each | self checkBackReferencesTo: each]