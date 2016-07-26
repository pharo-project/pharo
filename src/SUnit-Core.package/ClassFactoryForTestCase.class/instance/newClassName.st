creating
newClassName
	| postFix |
	postFix := self nextCount printString.
	^(#ClassForTestToBeDeleted, postFix) asSymbol.