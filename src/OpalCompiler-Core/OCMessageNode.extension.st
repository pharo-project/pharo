Extension { #name : 'OCMessageNode' }

{ #category : '*OpalCompiler-Core' }
OCMessageNode >> isInlineAndOr [

	self methodCompilationContextOrNil ifNotNil:[:c | c optionInlineAndOr ifFalse: [ ^false ]].
	self receiver isBlock ifTrue: [^ false].
	self isCascaded ifTrue: [^ false].
	(#(and: or:) includes: self selector) ifFalse: [^ false].
	^self arguments allSatisfy: [ :each |  each isBlock and: [each arguments isEmpty ]]
]

{ #category : '*OpalCompiler-Core' }
OCMessageNode >> isInlineCase [
	self methodCompilationContextOrNil ifNotNil:[:c | c optionInlineCase ifFalse: [ ^false ]].
	self isCascaded ifTrue: [^ false].
	(#(caseOf: caseOf:otherwise:) includes: self selector) ifFalse: [^ false].
	self arguments size = 2 ifTrue: [  "otherwise block"
		self arguments last isBlock ifFalse: [^ false]].
	self arguments first isDynamicArray ifFalse: [^ false].
	self arguments first statements ifEmpty: [^ false].
	self arguments first statements do: [:assoc |
		(assoc isMessage and: [assoc selector == #->]) ifFalse: [^ false].
		assoc receiver isBlock ifFalse: [^ false].
		assoc receiver arguments isEmpty ifFalse: [^ false].
		assoc arguments first isBlock ifFalse: [^ false].
	].
	^ true
]

{ #category : '*OpalCompiler-Core' }
OCMessageNode >> isInlineIf [

	self methodCompilationContextOrNil ifNotNil: [ :c |
		c optionInlineIf ifFalse: [ ^ false ] ].
	self receiver isBlock ifTrue: [ ^ false ].
	self isCascaded ifTrue: [ ^ false ].
	(#( #ifTrue: #ifFalse: #ifTrue:ifFalse: #ifFalse:ifTrue: ) includes: self selector) ifFalse: [ ^ false ].
	^ self arguments allSatisfy: [ :node | node isBlock and: [ node arguments isEmpty ]]
]

{ #category : '*OpalCompiler-Core' }
OCMessageNode >> isInlineIfNil [

	| assertNone assertOneOrNone |

	self methodCompilationContextOrNil ifNotNil:[:c | c optionInlineIfNil ifFalse: [ ^false ]].
	self receiver isBlock ifTrue: [^ false].
	self isCascaded ifTrue: [^ false].
	(#(ifNil: ifNotNil: ifNil:ifNotNil: ifNotNil:ifNil:) includes: self selector) ifFalse: [^ false].
	self arguments do: [:node | node isBlock ifFalse: [^ false]].
	assertNone := [:block |
		block arguments isEmpty ifFalse: [^ false]
	].
	assertOneOrNone := [:block |
		block arguments size > 1 ifTrue: [^ false]
	].
	self selector == #ifNil:
		ifTrue: [assertNone value: self arguments first].
	self selector == #ifNil:ifNotNil:
		ifTrue: [assertNone value: self arguments first.  assertOneOrNone value: self arguments last].
	self selector == #ifNotNil:
		ifTrue: [assertOneOrNone value: self arguments first].
	self selector == #ifNotNil:ifNil:
		ifTrue: [assertOneOrNone value: self arguments first. assertNone value: self arguments last].
	^ true
]

{ #category : '*OpalCompiler-Core' }
OCMessageNode >> isInlineRepeat [
	self methodCompilationContextOrNil ifNotNil:[:c | c optionInlineRepeat
		ifFalse: [ ^ false ]].
	self receiver isBlock
		ifFalse: [ ^ false ].
	self receiver arguments ifNotEmpty: [ ^ false ].
	self isCascaded
		ifTrue: [ ^ false ].
	#repeat = self selector
		ifFalse: [ ^ false ].
	^ true
]

{ #category : '*OpalCompiler-Core' }
OCMessageNode >> isInlineTimesRepeat [

	| block |
	self methodCompilationContextOrNil ifNotNil:[:c | c optionInlineTimesRepeat ifFalse: [ ^false ]].

	self receiver isBlock ifTrue: [^ false].
	self isCascaded ifTrue: [^ false].
	#timesRepeat: = self selector ifFalse: [^ false].
	block := self arguments last.
	block isBlock ifFalse: [^ false].
	block arguments ifNotEmpty: [^ false].
	^ true
]

{ #category : '*OpalCompiler-Core' }
OCMessageNode >> isInlineToDo [

	| block step |
	self methodCompilationContextOrNil ifNotNil:[:c | c optionInlineToDo ifFalse: [ ^false ]].

	self receiver isBlock ifTrue: [^ false].
	self isCascaded ifTrue: [^ false].
	(#(to:do: to:by:do:) includes: self selector) ifFalse: [^ false].
	block := self arguments last.
	block isBlock ifFalse: [^ false].
	block arguments size = 1 ifFalse: [^ false].
	self arguments first isVariable ifTrue: [
		(OCParseTreeSearcher new
			matches: self arguments first name , ' := `@object' do: [:n :a | true];
			executeTree: block initialAnswer: false) ifTrue: [^ false].
	].
	self arguments size = 3 "to:by:do:" ifTrue: [
		step := self arguments second.
		(step isLiteralNode and: [step value isNumber])ifFalse: [^ false].
		step value = 0 ifTrue: [^ false].
	].
	^ true
]

{ #category : '*OpalCompiler-Core' }
OCMessageNode >> isInlineWhile [

	self methodCompilationContextOrNil ifNotNil:[:c | c optionInlineWhile ifFalse: [ ^false ]].
	self isCascaded ifTrue: [^ false].
	(#(whileFalse: whileTrue: whileFalse whileTrue) includes: self selector) ifFalse: [^ false].
	self receiver isBlock ifFalse: [^ false].
	self receiver arguments isEmpty ifFalse: [^ false].
	self arguments isEmpty ifFalse: [
		self arguments first isBlock ifFalse: [^ false].
		self arguments first arguments isEmpty ifFalse: [^ false].
	].
	^ true
]

{ #category : '*OpalCompiler-Core' }
OCMessageNode >> isInlined [
	self methodCompilationContextOrNil ifNotNil:[:c | c optionInlineNone ifTrue: [ ^false ]].
	self isInlineIf ifTrue: [^true].
	self isInlineIfNil  ifTrue: [^true].
	self isInlineAndOr  ifTrue: [^true].
	self isInlineWhile ifTrue: [^true].
	self isInlineToDo  ifTrue: [^true].
	self isInlineCase  ifTrue: [^true].
	self isInlineTimesRepeat ifTrue: [^true].
	self isInlineRepeat ifTrue: [^true].
	^false
]

{ #category : '*OpalCompiler-Core' }
OCMessageNode >> methodCompilationContextOrNil [
	^ self methodNode ifNotNil: [ :node | node compilationContext ]
]
