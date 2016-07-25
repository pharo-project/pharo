class
defineTrait: defString notifying: aController  

	| defTokens keywdIx envt oldTrait newTraitName trait |
	self selectedClass isTrait ifTrue:[oldTrait := self selectedClass].
	defTokens := defString findTokens: Character separators.
	keywdIx := defTokens findFirst: [:x | x = 'category'].
	envt := self class environment.
	keywdIx := defTokens findFirst: [:x | x = 'named:'].
	newTraitName := (defTokens at: keywdIx+1) copyWithoutAll: '#()'.
	((oldTrait isNil or: [oldTrait baseTrait name asString ~= newTraitName])
		and: [envt includesKey: newTraitName asSymbol]) ifTrue:
			["Attempting to define new class/trait over existing one when
				not looking at the original one in this browser..."
			(self confirm: ((newTraitName , ' is an existing class/trait in this system.
Redefining it might cause serious problems.
Is this really what you want to do?') asText makeBoldFrom: 1 to: newTraitName size))
				ifFalse: [^ nil ]].

	trait := self class compiler source: defString; requestor: aController; logged: true; evaluate.
	^ trait