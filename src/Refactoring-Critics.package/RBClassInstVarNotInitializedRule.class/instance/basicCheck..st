running
basicCheck: aClass 
	| definesVar class |
	aClass isMeta ifFalse: [ ^ false ]. 
	class := aClass.
		definesVar := false.
		[ definesVar or: [ class isNil or: [ class isMeta not ] ] ] whileFalse: [
			definesVar := class instVarNames isNotEmpty
				"TestCase defines Announcers but does not initialize it -> all tests are reported
				so we exclude it here"
				and: [ class ~= TestCase class
				and: [ aClass ~= TestCase ] ].
			class := class superclass ].
	^ (definesVar and: [ (aClass includesSelector: #initialize) not ])