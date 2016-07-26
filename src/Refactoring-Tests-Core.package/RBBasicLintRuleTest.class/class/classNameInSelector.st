miscellaneous
classNameInSelector
	| detector |
	detector := self new.
	detector name: 'Redundant class name in selector'.
	detector methodBlock: 
			[:context :result | 
			(context selectedClass isMetaclass and: 
					[(context selector
						indexOfSubCollection: context selectedClass theNonMetaClass name
						startingAt: 1) > 0])
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector