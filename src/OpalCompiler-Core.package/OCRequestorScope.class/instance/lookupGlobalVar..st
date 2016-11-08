lookup
lookupGlobalVar: aName
	"consider variables with upper case as global vars resolveable in the outer scope "

	^ aName first isUppercase
		ifTrue: [ outerScope lookupVar: aName ]
		ifFalse: [ nil ]