"
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
A match start optimizer, handy for searching a string. Takes a regex syntax tree and sets itself up so that prefix characters or matcher states that cannot start a match are later recognized with #canStartMatch:in: method.

Used by RxMatcher, but can be used by other matchers (if implemented) as well.
"
Class {
	#name : #RxMatchOptimizer,
	#superclass : #Object,
	#instVars : [
		'ignoreCase',
		'prefixes',
		'nonPrefixes',
		'conditions',
		'testBlock',
		'methodPredicates',
		'nonMethodPredicates',
		'predicates',
		'nonPredicates',
		'lookarounds'
	],
	#category : #'Regex-Core-Utilities'
}

{ #category : #accessing }
RxMatchOptimizer >> canStartMatch: aCharacter in: aMatcher [

	"Answer whether a match could commence at the given lookahead
	character, or in the current state of <aMatcher>. True answered
	by this method does not mean a match will definitly occur, while false
	answered by this method *does* guarantee a match will never occur."

	aCharacter ifNil: [ ^ true ].
	^ testBlock == nil or: [ testBlock value: aCharacter value: aMatcher ]
]

{ #category : #accessing }
RxMatchOptimizer >> conditionTester [
	"#any condition is filtered at the higher level;
	it cannot appear among the conditions here."

	| matchCondition |
	conditions isEmpty ifTrue: [^nil].
	conditions size = 1 ifTrue:
		[matchCondition := conditions detect: [:ignored | true].
		"Special case all of the possible conditions."
		#atBeginningOfLine = matchCondition ifTrue: [^[:c :matcher | matcher atBeginningOfLine]].
		#atEndOfLine = matchCondition ifTrue: [^[:c :matcher | matcher atEndOfLine]].
		#atBeginningOfWord = matchCondition ifTrue: [^[:c :matcher | matcher atBeginningOfWord]].
		#atEndOfWord = matchCondition ifTrue: [^[:c :matcher | matcher atEndOfWord]].
		#atWordBoundary = matchCondition ifTrue: [^[:c :matcher | matcher atWordBoundary]].
		#notAtWordBoundary = matchCondition ifTrue: [^[:c :matcher | matcher notAtWordBoundary]].
		RxParser signalCompilationException: 'invalid match condition'].
	"More than one condition. Capture them as an array in scope."
	matchCondition := conditions asArray.
	^[:c :matcher |
		matchCondition contains:
			[:conditionSelector |
			matcher perform: conditionSelector]]
]

{ #category : #private }
RxMatchOptimizer >> determineTestMethod [
	"Answer a block closure that will work as a can-match predicate.
	Answer nil if no viable optimization is possible (too many chars would
	be able to start a match)."

	| testers |
	(conditions includes: #any) ifTrue: [^nil].
	testers := OrderedCollection new: 5.
	#(#prefixTester #nonPrefixTester #conditionTester #methodPredicateTester #nonMethodPredicateTester #predicateTester #nonPredicateTester)
		do: 
			[:selector | 
			| tester |
			tester := self perform: selector.
			tester notNil ifTrue: [testers add: tester]].
	testers isEmpty ifTrue: [^nil].
	testers size = 1 ifTrue: [^testers first].
	testers := testers asArray.
	^[:char :matcher | testers contains: [:t | t value: char value: matcher]]
]

{ #category : #initialization }
RxMatchOptimizer >> initialize: aRegex ignoreCase: aBoolean [ 
	"Set `testMethod' variable to a can-match predicate block:
	two-argument block which accepts a lookahead character
	and a matcher (presumably built from aRegex) and answers 
	a boolean indicating whether a match could start at the given
	lookahead. "

	ignoreCase := aBoolean.
	prefixes := Set new: 10.
	nonPrefixes := Set new: 10.
	conditions := Set new: 3.
	methodPredicates := Set new: 3.
	nonMethodPredicates := Set new: 3.
	predicates := Set new: 3.
	nonPredicates := Set new: 3.
	lookarounds := Set new: 3.
	aRegex dispatchTo: self.	"If the whole expression is nullable, 
		end-of-line is an implicit can-match condition!"
	aRegex isNullable ifTrue: [conditions add: #atEndOfLine].
	testBlock := self determineTestMethod
]

{ #category : #accessing }
RxMatchOptimizer >> methodPredicateTester [
	| p selector |
	methodPredicates isEmpty ifTrue: [^nil].
	p := self optimizeSet: methodPredicates.	"also allows copying closures"
	^p size = 1
		ifTrue: 
			["might be a pretty common case"
			selector := p first.
			[:char :matcher | 
			RxParser doHandlingMessageNotUnderstood:
				[char perform: selector]]]
		ifFalse: 
			[[:char :m | 
			RxParser doHandlingMessageNotUnderstood:
				[p contains: [:sel | char perform: sel]]]]
]

{ #category : #accessing }
RxMatchOptimizer >> nonMethodPredicateTester [
	| p selector |
	nonMethodPredicates isEmpty ifTrue: [^nil].
	p := self optimizeSet: nonMethodPredicates.	"also allows copying closures"
	^p size = 1
		ifTrue: 
			[selector := p first.
			[:char :matcher | 
			RxParser doHandlingMessageNotUnderstood:
				[(char perform: selector) not]]]
		ifFalse: 
			[[:char :m | 
			RxParser doHandlingMessageNotUnderstood:
				[p contains: [:sel | (char perform: sel) not]]]]
]

{ #category : #private }
RxMatchOptimizer >> nonPredicateTester [

	| p pred |
	nonPredicates isEmpty ifTrue: [^nil].
	p := self optimizeSet: nonPredicates.	"also allows copying closures"
	^p size = 1
		ifTrue: 
			[pred := p first.
			[:char :matcher | (pred value: char) not]]
		ifFalse: 
			[[:char :m | p contains: [:some | (some value: char) not]]]
]

{ #category : #private }
RxMatchOptimizer >> nonPrefixTester [

	| np nonPrefixChar |
	nonPrefixes isEmpty ifTrue: [^nil].
	np := self optimizeSet: nonPrefixes. "also allows copying closures"
	^np size = 1 "might be be pretty common case"
		ifTrue: 
			[nonPrefixChar := np first.
			[:char :matcher | char ~= nonPrefixChar]]
		ifFalse: [[:char : matcher | (np includes: char) not]]
]

{ #category : #private }
RxMatchOptimizer >> optimizeSet: aSet [
	"If a set is small, convert it to array to speed up lookup
	(Array has no hashing overhead, beats Set on small number
	of elements)."

	^aSet size < 10 ifTrue: [aSet asArray] ifFalse: [aSet]
]

{ #category : #private }
RxMatchOptimizer >> predicateTester [

	| p pred |
	predicates isEmpty ifTrue: [^nil].
	p := self optimizeSet: predicates.	"also allows copying closures"
	^p size = 1
		ifTrue: [
			pred := p first.
			[:char :matcher | 
				pred value: char ]]
		ifFalse: [
			[:char :matcher | 
				p contains: [:some | some value: char ]]]
]

{ #category : #private }
RxMatchOptimizer >> prefixTester [

	| p prefixChar |
	prefixes isEmpty ifTrue: [^nil].
	p := self optimizeSet: prefixes. "also allows copying closures"
	ignoreCase ifTrue: [p := p collect: [:each | each asUppercase]].
	^p size = 1 "might be a pretty common case"
		ifTrue: 
			[prefixChar := p first.
			ignoreCase
				ifTrue: [[:char :matcher | char sameAs: prefixChar]]
				ifFalse: [[:char :matcher | char = prefixChar]]]
		ifFalse:
			[ignoreCase
				ifTrue: [[:char :matcher | p includes: char asUppercase]]
				ifFalse: [[:char :matcher | p includes: char]]]
]

{ #category : #'double dispatch' }
RxMatchOptimizer >> syntaxAny [
	"Any special char is among the prefixes."

	conditions add: #any
]

{ #category : #'double dispatch' }
RxMatchOptimizer >> syntaxBeginningOfLine [
	"Beginning of line is among the prefixes."

	conditions add: #atBeginningOfLine
]

{ #category : #'double dispatch' }
RxMatchOptimizer >> syntaxBeginningOfWord [
	"Beginning of line is among the prefixes."

	conditions add: #atBeginningOfWord
]

{ #category : #'double dispatch' }
RxMatchOptimizer >> syntaxBranch: branchNode [
	"If the head piece of the branch is transparent (allows 0 matches),
	we must recurse down the branch. Otherwise, just the head atom
	is important."

	(branchNode piece isNullable and: [branchNode branch notNil])
		ifTrue: [branchNode branch dispatchTo: self].
	branchNode piece dispatchTo: self
]

{ #category : #'double dispatch' }
RxMatchOptimizer >> syntaxCharSet: charSetNode [ 
	"All these (or none of these) characters is the prefix."

	charSetNode isNegated
		ifTrue: [nonPrefixes addAll: (charSetNode enumerableSetIgnoringCase: ignoreCase)]
		ifFalse: [prefixes addAll: (charSetNode enumerableSetIgnoringCase: ignoreCase)].
	charSetNode hasPredicates ifTrue: 
			[charSetNode isNegated
				ifTrue: [nonPredicates addAll: charSetNode predicates]
				ifFalse: [predicates addAll: charSetNode predicates]]
]

{ #category : #'double dispatch' }
RxMatchOptimizer >> syntaxCharacter: charNode [
	"This character is the prefix, of one of them."

	prefixes add: charNode character
]

{ #category : #'double dispatch' }
RxMatchOptimizer >> syntaxEndOfLine [
	"Beginning of line is among the prefixes."

	conditions add: #atEndOfLine
]

{ #category : #'double dispatch' }
RxMatchOptimizer >> syntaxEndOfWord [

	conditions add: #atEndOfWord
]

{ #category : #'double dispatch' }
RxMatchOptimizer >> syntaxEpsilon [
	"Empty string, terminate the recursion (do nothing)."
]

{ #category : #'double dispatch' }
RxMatchOptimizer >> syntaxLookaround: lookaroundNode [ 

	
	lookarounds add: lookaroundNode
]

{ #category : #'double dispatch' }
RxMatchOptimizer >> syntaxMessagePredicate: messagePredicateNode [ 
	messagePredicateNode negated
		ifTrue: [nonMethodPredicates add: messagePredicateNode selector]
		ifFalse: [methodPredicates add: messagePredicateNode selector]
]

{ #category : #'double dispatch' }
RxMatchOptimizer >> syntaxNonWordBoundary [

	conditions add: #notAtWordBoundary
]

{ #category : #'double dispatch' }
RxMatchOptimizer >> syntaxPiece: pieceNode [
	"Pass on to the atom."

	pieceNode atom dispatchTo: self
]

{ #category : #'double dispatch' }
RxMatchOptimizer >> syntaxPredicate: predicateNode [ 

	predicates add: predicateNode predicate
]

{ #category : #'double dispatch' }
RxMatchOptimizer >> syntaxRegex: regexNode [
	"All prefixes of the regex's branches should be combined.
	Therefore, just recurse."

	regexNode branch dispatchTo: self.
	regexNode regex notNil
		ifTrue: [regexNode regex dispatchTo: self]
]

{ #category : #'double dispatch' }
RxMatchOptimizer >> syntaxWordBoundary [

	conditions add: #atWordBoundary
]
