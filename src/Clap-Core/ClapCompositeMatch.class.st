"
I am a match for a composite parameter specification, and thus cover a (possibly empty) range of words in the arguments sequence.

I provide methods for querying and accessing the matches of my specification's children parameters.
"
Class {
	#name : #ClapCompositeMatch,
	#superclass : #ClapExplicit,
	#instVars : [
		'children'
	],
	#category : #'Clap-Core-Activation'
}

{ #category : #matching }
ClapCompositeMatch >> addChild: aSubExpression [
	children add: aSubExpression
]

{ #category : #'accessing - children' }
ClapCompositeMatch >> at: aSpec [
	^ self
		at: aSpec
		ifAbsent: [ ClapImplicit of: aSpec in: self ]
]

{ #category : #'accessing - children' }
ClapCompositeMatch >> at: aSpec ifAbsent: absentBlock [
	^ children
		detect: [ :child | child specification = aSpec ]
		ifNone: absentBlock
]

{ #category : #'accessing - children' }
ClapCompositeMatch >> at: aSpec ifFound: foundBlock [
	^ children
		detect: [ :child | child specification = aSpec ]
		ifFound: foundBlock
]

{ #category : #'accessing - children' }
ClapCompositeMatch >> at: aSpec ifFound: foundBlock ifAbsent: absentBlock [
	^ children
		detect: [ :child | child specification = aSpec ]
		ifFound: foundBlock
		ifNone: absentBlock
]

{ #category : #'accessing - children' }
ClapCompositeMatch >> atName: canonicalName [
	^ self at: (specification atName: canonicalName)
]

{ #category : #'accessing - children' }
ClapCompositeMatch >> atName: canonicalName ifFound: foundBlock [
	^ self
		at: (specification atName: canonicalName)
		ifFound: foundBlock
]

{ #category : #'accessing - children' }
ClapCompositeMatch >> atName: canonicalName ifFound: foundBlock ifAbsent: absentBlock [
	^ self
		at: (specification atName: canonicalName)
		ifFound: foundBlock
		ifAbsent: absentBlock
]

{ #category : #matching }
ClapCompositeMatch >> completeMatchOn: aStream [
	startIndex := aStream position + 1.
	self matchChildrenOn: aStream
]

{ #category : #matching }
ClapCompositeMatch >> detectMatchOn: aStream ifFound: foundBlock ifNone: noneBlock [
	self specification parametersDo: [ :param |
		(param isMultiple or: (self includesMatchOf: param) not)
			ifTrue: [ | sub |
				sub := param matchOn: aStream.
				sub isMismatch
					ifFalse: [ ^ foundBlock cull: sub ] ] ].
	^ noneBlock value
]

{ #category : #'accessing - children' }
ClapCompositeMatch >> flags [
	^ children select: [ :each | specification flags includes: each specification ]
]

{ #category : #testing }
ClapCompositeMatch >> includesMatchNamed: canonicalName [
	^ children anySatisfy: [ :arg | arg specification canonicalName = canonicalName ]
]

{ #category : #testing }
ClapCompositeMatch >> includesMatchOf: aSpec [
	^ children anySatisfy: [ :arg | arg specification = aSpec ]
]

{ #category : #initialization }
ClapCompositeMatch >> initialize [
	children := OrderedCollection new
]

{ #category : #matching }
ClapCompositeMatch >> matchChildrenOn: aStream [
	[ self
		detectMatchOn: aStream
		ifFound: [ :arg | arg recordIn: self ]
		ifNone: [ ^ self ]
	] repeat
]

{ #category : #'accessing - children' }
ClapCompositeMatch >> matchedSubcommand: foundBlock ifNone: noneBlock [
	^ children
		detect: [ :child | specification subcommands includes: child specification ]
		ifFound: [ :cmdMatch | foundBlock cull: cmdMatch ]
		ifNone: noneBlock
]

{ #category : #'accessing - children' }
ClapCompositeMatch >> occurrencesNamed: canonicalName [
	^ self occurrencesOf: (specification atName: canonicalName)
]

{ #category : #enumerating }
ClapCompositeMatch >> occurrencesNamed: canonicalName collect: aBlock [
	^ self
		occurrencesOf: (specification atName: canonicalName)
		collect: aBlock
]

{ #category : #enumerating }
ClapCompositeMatch >> occurrencesNamed: canonicalName do: aBlock [
	^ self
		occurrencesOf: (specification atName: canonicalName)
		do: aBlock
]

{ #category : #'accessing - children' }
ClapCompositeMatch >> occurrencesOf: aSpec [
	^ children select: [ :child | child specification = aSpec ]
]

{ #category : #enumerating }
ClapCompositeMatch >> occurrencesOf: aSpec collect: aBlock [
	^ children
		select: [ :child | child specification = aSpec ]
		thenCollect: aBlock
]

{ #category : #enumerating }
ClapCompositeMatch >> occurrencesOf: aSpec do: aBlock [
	^ children
		select: [ :child | child specification = aSpec ]
		thenDo: aBlock
]

{ #category : #evaluating }
ClapCompositeMatch >> positionalValues [
	^ specification positionals
		collect: [ :each | (self at: each) value ]
]

{ #category : #accessing }
ClapCompositeMatch >> stop [
	^ children
		ifEmpty: [ self start - 1 ]
		ifNotEmpty: [ children last stop ]
	
]

{ #category : #validation }
ClapCompositeMatch >> validateOn: aReport [
	super validateOn: aReport.
	children do: [ :each | each validateOn: aReport ]
]
