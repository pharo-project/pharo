Extension { #name : #ClySystemEnvironment }

{ #category : #'*Calypso-SystemPlugins-Traits-Queries' }
ClySystemEnvironment >> defineTrait: defString notifying: aController startingFrom: aClass [

	| defTokens keywdIx oldTrait newTraitName trait |
	aClass isTrait ifTrue:[oldTrait := aClass].
	defTokens := defString findTokens: Character separators.
	keywdIx := defTokens findFirst: [:x | x = 'category'].
	keywdIx := defTokens findFirst: [:x | x = 'named:'].
	newTraitName := (defTokens at: keywdIx+1) copyWithoutAll: '#()'.
	((oldTrait isNil or: [oldTrait instanceSide name asString ~= newTraitName])
		and: [self includesClassNamed: newTraitName asSymbol]) ifTrue:
			["Attempting to define new class/trait over existing one when
				not looking at the original one in this browser..."
			(self confirm: ((newTraitName , ' is an existing class/trait in this system.
Redefining it might cause serious problems.
Is this really what you want to do?') asText makeBoldFrom: 1 to: newTraitName size))
				ifFalse: [^ nil ]].

	trait := self defaultClassCompiler
		source: defString; requestor: aController; 
		logged: true; 
		evaluate.
	^trait isTrait ifTrue: [ trait ] ifFalse: [ nil ]
]
