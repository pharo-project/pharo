*NautilusCommon
correspondingClassTested
	"Returns the class tested by this TestCase or nil if it can't be found."

	| classNameString |
	classNameString := self name asString.
	^ self environment
		at:
			((classNameString endsWith: 'Test')
				ifTrue: [ classNameString allButLast: 4	"'Test' size" ]
				ifFalse: [ classNameString ]) asSymbol
		ifAbsent: [ nil ]