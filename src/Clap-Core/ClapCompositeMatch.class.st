"
I am a match for a composite parameter specification, and thus cover a (possibly empty) range of words in the arguments sequence.

I provide methods for querying and accessing the matches of my specification's children parameters.
"
Class {
	#name : 'ClapCompositeMatch',
	#superclass : 'ClapExplicit',
	#instVars : [
		'children'
	],
	#category : 'Clap-Core-Activation',
	#package : 'Clap-Core',
	#tag : 'Activation'
}

{ #category : 'matching' }
ClapCompositeMatch >> addChild: aSubExpression [
	children add: aSubExpression
]

{ #category : 'accessing - children' }
ClapCompositeMatch >> at: identifierOrSpec [
	^ self at: identifierOrSpec
		ifAbsent: [
			specification at: identifierOrSpec
				ifPresent: [ :spec | ClapImplicit of: spec in: self ]
				ifAbsent: [ NotFound signal: 'Cannot find ' , identifierOrSpec asSymbol, '!' ] ] 	
]

{ #category : 'accessing - children' }
ClapCompositeMatch >> at: identifierOrSpec ifAbsent: absentBlock [
	^ children
		detect: [ :child | identifierOrSpec identifiesClapParameter: child specification ]
		ifNone: absentBlock
]

{ #category : 'accessing - children' }
ClapCompositeMatch >> at: identifierOrSpec ifPresent: foundBlock [
	^ children
		detect: [ :child | identifierOrSpec identifiesClapParameter: child specification ]
		ifFound: foundBlock
]

{ #category : 'accessing - children' }
ClapCompositeMatch >> at: identifierOrSpec ifPresent: foundBlock ifAbsent: absentBlock [
	^ children
		detect: [ :child | identifierOrSpec identifiesClapParameter: child specification ]
		ifFound: foundBlock
		ifNone: absentBlock
]

{ #category : 'matching' }
ClapCompositeMatch >> completeMatchOn: aStream [
	startIndex := aStream position + 1.
	self matchChildrenOn: aStream
]

{ #category : 'matching' }
ClapCompositeMatch >> detectMatchOn: aStream ifFound: foundBlock ifNone: noneBlock [
	self specification
		parametersSelect: [ :each | each isMatchableIn: self ]
		thenDo: [ :param | 
			| sub |
			sub := param matchOn: aStream in: self context.
			sub isMismatch
				ifFalse: [ ^ foundBlock cull: sub ] ].
	^ noneBlock value
]

{ #category : 'accessing - children' }
ClapCompositeMatch >> flags [
	^ children select: [ :each | specification flags includes: each specification ]
]

{ #category : 'testing' }
ClapCompositeMatch >> hasChild [

	^ children isNotEmpty
]

{ #category : 'testing' }
ClapCompositeMatch >> includesMatchOf: identifierOrSpec [
	^ children anySatisfy: [ :arg | arg isMatchOf: identifierOrSpec ]
]

{ #category : 'initialization' }
ClapCompositeMatch >> initialize [
	children := OrderedCollection new
]

{ #category : 'validation' }
ClapCompositeMatch >> isValid: aValidation [

	^ aValidation isValidCompositeMatch: self
]

{ #category : 'matching' }
ClapCompositeMatch >> matchChildrenOn: aStream [
	[ self
		detectMatchOn: aStream
		ifFound: [ :arg | arg recordIn: self ]
		ifNone: [ ^ self ]
	] repeat
]

{ #category : 'accessing - children' }
ClapCompositeMatch >> matchedSubcommand: foundBlock ifNone: noneBlock [
	^ children
		detect: [ :child | specification subcommands includes: child specification ]
		ifFound: [ :cmdMatch | foundBlock cull: cmdMatch ]
		ifNone: noneBlock
]

{ #category : 'accessing - children' }
ClapCompositeMatch >> occurrencesOf: identifierOrSpec [
	^ children select: [ :child | identifierOrSpec identifiesClapParameter: child specification ]
]

{ #category : 'enumerating' }
ClapCompositeMatch >> occurrencesOf: identifierOrSpec collect: aBlock [
	^ children
		select: [ :child | identifierOrSpec identifiesClapParameter: child specification ]
		thenCollect: aBlock
]

{ #category : 'enumerating' }
ClapCompositeMatch >> occurrencesOf: identifierOrSpec do: aBlock [
	^ children
		select: [ :child | identifierOrSpec identifiesClapParameter: child specification ]
		thenDo: aBlock
]

{ #category : 'evaluating' }
ClapCompositeMatch >> positionalValues [
	^ specification positionals
		collect: [ :each | (self at: each) value ]
]

{ #category : 'accessing' }
ClapCompositeMatch >> stop [
	^ children
		ifEmpty: [ self start - 1 ]
		ifNotEmpty: [ children last stop ]
]

{ #category : 'accessing - children' }
ClapCompositeMatch >> subcommand [
	"answer the direct subcommand if any"
	
	^ children 
		detect: [ :each | each specification isCommand ]
]

{ #category : 'accessing - children' }
ClapCompositeMatch >> subcommands [
	| commands |
	
	commands := OrderedCollection new.
	commands add: self.
	[ [ commands add: commands last subcommand ] repeat ]
	on: NotFound, SubscriptOutOfBounds
	do: [ "we stop the recursion" ].
	
	^ commands first specification isCommand
		ifTrue: [ commands ]
		ifFalse: [ commands copyWithoutFirst ]
]

{ #category : 'validation' }
ClapCompositeMatch >> validate: aClapValidationClass on: aReport [
	super validate: aClapValidationClass on: aReport.
	children do: [ :each | each validate: aClapValidationClass on: aReport ]
]

{ #category : 'validation' }
ClapCompositeMatch >> validateOn: aReport [
	super validateOn: aReport.
	children do: [ :each | each validateOn: aReport ]
]
