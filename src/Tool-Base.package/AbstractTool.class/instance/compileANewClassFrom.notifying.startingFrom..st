class
compileANewClassFrom: aString notifying: aController startingFrom: aClass
	
	" Copied from Browser "
	
	"The receiver's textual content is a request to define a new class. The
	source code is defString. If any errors occur in compilation, notify
	aController."
	| oldClass class newClassName defTokens keywdIx envt |
	oldClass := aClass.
	defTokens := aString findTokens: Character separators.
	
	((defTokens first = 'Trait' and: [defTokens second = 'named:'])
		or: [defTokens second = 'classTrait'])
		ifTrue: [^ self defineTrait: aString notifying: aController ].
		
	keywdIx := defTokens findFirst: [:x | x beginsWith: 'category'].
	envt := Smalltalk globals.
	keywdIx := defTokens findFirst: [:x | '*subclass*' match: x].
	newClassName := (defTokens at: keywdIx+1) copyWithoutAll: '#()'.
	((oldClass isNil or: [oldClass theNonMetaClass name asString ~= newClassName])
		and: [envt includesKey: newClassName asSymbol]) ifTrue:
			["Attempting to define new class over existing one when
				not looking at the original one in this browser..."
			(self confirm: ((newClassName , ' is an existing class in this system.
Redefining it might cause serious problems.
Is this really what you want to do?') asText makeBoldFrom: 1 to: newClassName size))
				ifFalse: [^ nil]].
	"ar 8/29/1999: Use oldClass superclass for defining oldClass
	since oldClass superclass knows the definerClass of oldClass."
	oldClass ifNotNil:[oldClass := oldClass superclass].
	class := oldClass subclassDefinerClass new
				source: aString;
				requestor: aController;
				logged: true;
				evaluate.
	^ (class isKindOf: Behavior)
		ifTrue: [ class ]
		ifFalse: [ nil ]