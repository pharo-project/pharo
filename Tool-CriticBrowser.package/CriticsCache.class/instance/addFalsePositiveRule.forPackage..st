add/remove
addFalsePositiveRule: aRule forPackage: aPackage
	
	| fp |
	(falsePositiveRules includesKey: aPackage) 
		ifFalse: [ falsePositiveRules at: aPackage put: Set new ].
	(falsePositiveRules at: aPackage) add: (aRule class uniqueIdentifierName). 
	fp := (critics at: aRule ifAbsent: [^ self])
				select: [ :c |  (self packageOf: c) package name = aPackage packageName ].	
	fp do: [ :c | self addFalsePositive: c forRule: aRule ]