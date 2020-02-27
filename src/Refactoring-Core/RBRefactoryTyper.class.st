"
I am a type guesser used by refactoring operations.

I try to determine the types used for variables in a class.

Refactoring operations are using me to provide a possible list of classes that are affected by a refactoring. 
For example, for moving or delegating a method implementation to an instance variable, it might be necessary to add this implementation to multiple classes.

I analyze message sends to the instance variables and try to guess the type by looking at implementors of that messages. 
(If a method sends ifTrue:ifFalse: to an instance variable I can guess that the instance variable may be a Boolean object).

If I can guess the type to be a collection, I'll try to investigate the types of the possible contained elements as well.
You can ask for the guessed type of the variable: 
typer guessTypeFor: 'variablename'
and the contained elements: 
typer guessTypeFor: '-variablename-'

Example usage: 
| typer |
""create and initialize for a class to analyze""
typer := (RBRefactoryTyper new runOn: RBMessageNode).

""guess types for RBMessageNodes instane var 'arguments' ""
typer  guessTypesFor:'arguments'.  ""a Set(SequenceableCollection)""

""guess types for objects that may be put  into  'arguments' collection ""
typer  guessTypesFor:'-arguments-' ""a Set(RBBlockNode RBMethodNode)""


You can print a full report on all results with #printString.

(RBRefactoryTyper new runOn: Point ) printString.
""'Point
	x	<Integer>
	y	<Integer>
'""

There are two class side methods, one for creating an instance and setting the environment used to resolve names - a RBNamespace.

And another one for guessing types in a parseTree:

RBRefactoryTyper typesFor: 'var' in: (RBParser parseExpression: 'var squared') model: RBNamespace new ""a Set(Number Collection)""
(Here, I can guess the types Number and Collections as these are the classes implementing the message #squared)


"
Class {
	#name : #RBRefactoryTyper,
	#superclass : #Object,
	#instVars : [
		'model',
		'class',
		'variableTypes',
		'bestGuesses',
		'variableMessages',
		'backpointers',
		'methodName',
		'selectorLookup'
	],
	#category : #'Refactoring-Core-Support'
}

{ #category : #'instance creation' }
RBRefactoryTyper class >> newFor: aRBNamespace [ 
	^ self new
			model: aRBNamespace;
			yourself
]

{ #category : #accessing }
RBRefactoryTyper class >> typesFor: variableName in: aParseTree model: aRBSmalltalk [ 
	| searcher messages |
	searcher := RBParseTreeSearcher new.
	searcher matches: variableName , ' `@message: ``@args'
		do: 
			[:aNode :answer | 
			answer
				add: aNode selector;
				yourself].
	messages := searcher executeTree: aParseTree initialAnswer: Set new.
	^(self new)
		model: aRBSmalltalk;
		findTypeFor: messages
]

{ #category : #private }
RBRefactoryTyper >> backpointersDictionary [
	"Create a special dictionary, because the host systems wrongly treats #abc and 'abc' as equal."

	^ PluggableDictionary new
		equalBlock: [ :a :b | a class == b class and: [ a = b ] ];
		hashBlock: [ :a | a class identityHash bitXor: a hash ];
		yourself
]

{ #category : #private }
RBRefactoryTyper >> backpointersSetWith: anObject [
	"Create a special set, because the host systems wrongly treats #abc and 'abc' as equal."
	
	^ PluggableSet new
		equalBlock: [ :a :b | a class == b class and: [ a = b ] ];
		hashBlock: [ :a | a class identityHash bitXor: a hash ];
		add: anObject;
		yourself
]

{ #category : #printing }
RBRefactoryTyper >> collectionNameFor: aString [ 
	^'-<1s>-' expandMacrosWith: aString
]

{ #category : #'equivalence classes' }
RBRefactoryTyper >> computeEquivalenceClassesForMethodsAndVars [
	| searcher |
	backpointers := self backpointersDictionary.
	class instanceVariableNames
		do: [ :each | backpointers at: each put: (self backpointersSetWith: each) ].
	class withAllSubclasses
		do: [ :sub | 
			sub selectors
				do: [ :each | backpointers at: each put: (self backpointersSetWith: each) ] ].
	searcher := self parseTreeSearcher.
	searcher
		matches: '^``@object'
		do: [ :aNode :answer | self processNode: aNode value ].
	self executeSearch: searcher
]

{ #category : #selectors }
RBRefactoryTyper >> computeMessagesSentToVariables [
	| searcher |
	variableMessages := Dictionary new.
	class instanceVariableNames
		do: [ :each | variableMessages at: each put: Set new ].
	searcher := self parseTreeSearcher.
	class instanceVariableNames
		do: [ :each | 
			| block |
			block := [ :aNode :answer | 
			(variableMessages at: each ifAbsentPut: [ Set new ])
				add: aNode selector.
			self processCollectionMessagesFor: each in: aNode ].
			searcher matches: each , ' `@messageName: ``@args' do: block.
			(backpointers at: each)
				do: [ :sel | 
					sel isSymbol
						ifTrue: [ searcher
								matches:
									('(self <1s>) `@messageName: ``@args'
										expandMacrosWith: (self parseTreeSearcherClass buildSelectorString: sel))
										asString
								do: block ] ] ].
	searcher answer: variableMessages.
	self executeSearch: searcher
]

{ #category : #'computing types' }
RBRefactoryTyper >> computeTypes [
	variableMessages
		keysAndValuesDo: [:key :value | variableTypes at: key put: (self findTypeFor: value)]
]

{ #category : #private }
RBRefactoryTyper >> executeSearch: searcher [ 
	class withAllSubclasses do: 
			[:each | 
			each selectors do: 
					[:sel | 
					| parseTree |
					methodName := sel.
					parseTree := each parseTreeFor: sel.
					parseTree notNil ifTrue: [searcher executeTree: parseTree]]]
]

{ #category : #'computing types' }
RBRefactoryTyper >> findTypeFor: selectorCollection [ 
	^selectorCollection inject: self rootClasses
		into: 
			[:classes :each | 
			self refineTypes: classes
				with: (selectorLookup at: each ifAbsentPut: [self implementorsOf: each])]
]

{ #category : #assignments }
RBRefactoryTyper >> guessTypeFromAssignment: aNode [

	| type set newType |

	type := nil.
	aNode value isAssignment
		ifTrue: [ ^ self
				guessTypeFromAssignment: ( RBAssignmentNode variable: aNode variable value: aNode value value )
			].
	aNode value isBlock
		ifTrue: [ type := model classFor: [  ] class ].
	aNode value isLiteralNode
		ifTrue: [ aNode value value ifNil: [ ^ self ].
			type := model classFor: ( self typeFor: aNode value value )
			].
	aNode value isMessage
		ifTrue: [ aNode value receiver isVariable
				ifTrue: [ type := model classNamed: aNode value receiver name asSymbol ].
			aNode value selector = #asValue
				ifTrue: [ type := model classNamed: #ValueHolder ].
			( #(#and: #or: #= #== #~= #~~ #<= #< #~~ #> #>=) includes: aNode value selector )
				ifTrue: [ type := model classFor: Boolean ]
			].
	type ifNil: [ ^ self ].
	set := variableTypes at: aNode variable name.
	newType := set detect: [ :each | type includesClass: each ] ifNone: [ nil ].
	newType ifNil: [ ^ self ].
	( ( self rootClasses includes: newType ) or: [ newType = ( model classFor: Object ) ] )
		ifTrue: [ newType := type ].
	( bestGuesses at: aNode variable name ifAbsentPut: [ Set new ] ) add: newType
]

{ #category : #accessing }
RBRefactoryTyper >> guessTypesFor: anInstVarName [ 
	^bestGuesses at: anInstVarName ifAbsent: [self typesFor: anInstVarName]
]

{ #category : #accessing }
RBRefactoryTyper >> guessTypesFor: anInstVarName in: aClass [ 
	class = aClass ifFalse: [self runOn: aClass].
	^bestGuesses at: anInstVarName
		ifAbsent: [self typesFor: anInstVarName in: aClass]
]

{ #category : #'computing types' }
RBRefactoryTyper >> implementorsOf: aSelector [ 
	| classes |
	classes := OrderedCollection new.
	self rootClasses do: 
			[:each | 
			self 
				implementorsOf: aSelector
				in: each
				storeIn: classes].
	^classes
]

{ #category : #'computing types' }
RBRefactoryTyper >> implementorsOf: aSelector in: aClass storeIn: classes [ 
	(aClass directlyDefinesMethod: aSelector) 
		ifTrue: 
			[classes add: aClass.
			^self].
	aClass subclasses do: 
			[:each | 
			self 
				implementorsOf: aSelector
				in: each
				storeIn: classes]
]

{ #category : #initialization }
RBRefactoryTyper >> initialize [
	model := RBClassModelFactory rbNamespace new.
	class := model classFor: Object.
	variableTypes := Dictionary new.
	variableMessages := Dictionary new.
	selectorLookup := IdentityDictionary new.
	bestGuesses := Dictionary new
]

{ #category : #'equivalence classes' }
RBRefactoryTyper >> merge: aName [ 
	| set1 set2 |
	set1 := backpointers at: methodName ifAbsent: [nil].
	set2 := backpointers at: aName ifAbsent: [nil].
	(set1 isNil or: [set2 isNil or: [set1 == set2]]) ifTrue: [^self].
	set1 addAll: set2.
	set2 do: [:each | backpointers at: each put: set1]
]

{ #category : #private }
RBRefactoryTyper >> model [
	^model
]

{ #category : #private }
RBRefactoryTyper >> model: aRBSmalltalk [
	model := aRBSmalltalk
]

{ #category : #'selectors-collections' }
RBRefactoryTyper >> parseTreeSearcher [
	^ self parseTreeSearcherClass new
]

{ #category : #selectors }
RBRefactoryTyper >> parseTreeSearcherClass [
	^ RBParseTreeSearcher
]

{ #category : #printing }
RBRefactoryTyper >> printOn: aStream [ 
	aStream
		nextPutAll: class name;
		cr.
	class instanceVariableNames do: 
			[:each | 
			aStream
				tab;
				nextPutAll: each;
				tab;
				nextPut: $<.
			self printTypeFor: each on: aStream.
			aStream
				nextPut: $>;
				cr]
]

{ #category : #printing }
RBRefactoryTyper >> printType: aClass for: aString on: aStream [ 
	| name colTypes |
	colTypes := #().
	name := self collectionNameFor: aString.
	(aClass includesClass: (model classFor: Collection))
		ifTrue: [colTypes := self guessTypesFor: name].
	colTypes ifNotEmpty: [aStream nextPut: $(].
	aClass printOn: aStream.
	colTypes ifNotEmpty: 
			[aStream nextPutAll: ' of: '.
			colTypes size > 1 ifTrue: [aStream nextPut: $(].
			self printTypeFor: name on: aStream.
			colTypes size > 1 ifTrue: [aStream nextPut: $)]].
	colTypes ifNotEmpty: [aStream nextPut: $)]
]

{ #category : #printing }
RBRefactoryTyper >> printTypeFor: aString on: aStream [ 
	| types |
	types := (self guessTypesFor: aString) 
				asSortedCollection: [:a :b | a name < b name].
	1 to: types size
		do: 
			[:i | 
			i == 1 ifFalse: [aStream nextPutAll: ' | '].
			self 
				printType: (types at: i)
				for: aString
				on: aStream]
]

{ #category : #'selectors-collections' }
RBRefactoryTyper >> processCollectionFor: key messagesTo: aName in: aBlock [
	| searcher |
	searcher := self parseTreeSearcher.
	searcher
		matches: aName , ' `@message: ``@args'
		do: [ :aNode :answer | 
			self processCollectionMessagesFor: key in: aNode.
			answer
				add: aNode selector;
				yourself ].
	searcher
		executeTree: aBlock
		initialAnswer:
			(variableMessages
				at: (self collectionNameFor: key)
				ifAbsentPut: [ Set new ])
]

{ #category : #'selectors-collections' }
RBRefactoryTyper >> processCollectionMessagesFor: variableName in: aParseTree [ 
	| parent block |
	aParseTree isMessage
		ifFalse: [ ^ self ].
	(#(anyOne at: at:ifAbsent: at:ifAbsentPut: atPin: atRandom atRandom: atWrap: eight fifth first fourth last middle ninth second seventh sixth third) includes: aParseTree selector) ifTrue: [
		parent := aParseTree parent.
		(parent notNil and: [ parent isMessage ])
			ifFalse: [ ^ self ].
		aParseTree == parent receiver
			ifFalse: [ ^ self ].
		(variableMessages 
			at: (self collectionNameFor: variableName)
			ifAbsentPut: [Set new])
				add: parent selector.
		self processCollectionMessagesFor: (self collectionNameFor: variableName) in: parent ].
	(#(allSatisfy: anySatisfy: collect: collect:as: detect: detect:ifNone: detectMax: detectMin: detectSum: do: do:displayingProgress: do:separatedBy: #flatCollect: noneSatisfy: reject: select:) includes: aParseTree selector) ifTrue: [
		block := aParseTree arguments first.
		block isBlock ifFalse: [ ^ self ].
		self 
			processCollectionFor: variableName
			messagesTo: block arguments first name
			in: block ].
	(#(reduce: reduceLeft: reduceRight:) includes: aParseTree selector) ifTrue: [ 
		block := aParseTree arguments last.
		block isBlock ifFalse: [ ^ self ].
		block arguments do: [ :node |
			self
				processCollectionFor: variableName
				messagesTo: node name
				in: block ] ].
	#inject:into: = aParseTree selector ifTrue: [
		block := aParseTree arguments last.
		block isBlock ifFalse: [ ^ self ].
		self 
			processCollectionFor: variableName
			messagesTo: block arguments last name
			in: block ]
]

{ #category : #'equivalence classes' }
RBRefactoryTyper >> processNode: aNode [ 
	(aNode isVariable and: [class instanceVariableNames includes: aNode name]) 
		ifTrue: [^self merge: aNode name].
	(aNode isMessage 
		and: [aNode receiver isSelf]) 
			ifTrue: [^self merge: aNode selector].
	aNode isAssignment 
		ifTrue: 
			[self
				processNode: aNode value;
				processNode: aNode variable].
	(aNode isMessage and: 
			[#(#ifTrue: #ifFalse: #ifTrue:ifFalse: #ifFalse:ifTrue:) 
				includes: aNode selector]) 
		ifTrue: 
			[aNode arguments do: 
					[:each | 
					each isBlock 
						ifTrue: 
							[each body statements isEmpty 
								ifFalse: [self processNode: each body statements last]]]]
]

{ #category : #'computing types' }
RBRefactoryTyper >> refineTypes: aClassCollection with: anotherClassCollection [ 
	| classSet |
	classSet := Set new.
	aClassCollection do: 
			[:each | 
			anotherClassCollection do: 
					[:cls | 
					(cls includesClass: each) 
						ifTrue: [classSet add: cls]
						ifFalse: [(each includesClass: cls) ifTrue: [classSet add: each]]]].
	^classSet
]

{ #category : #assignments }
RBRefactoryTyper >> refineTypesByLookingAtAssignments [
	| searcher needsSearch |
	needsSearch := false.
	searcher := self parseTreeSearcher.
	variableTypes
		keysAndValuesDo: [ :key :value | 
			key first = $-
				ifFalse: [ needsSearch := true.
					searcher
						matches: key , ' := ``@object'
						do: [ :aNode :answer | self guessTypeFromAssignment: aNode ] ] ].
	needsSearch
		ifTrue: [ self executeSearch: searcher ]
]

{ #category : #private }
RBRefactoryTyper >> rootClasses [
	^ model rootClasses
]

{ #category : #accessing }
RBRefactoryTyper >> runOn: aClass [ 
	variableTypes := Dictionary new.
	variableMessages := Dictionary new.
	bestGuesses := Dictionary new.
	class := model classFor: aClass.
	class instanceVariableNames isEmpty ifTrue: [^self].
	self
		selectedClass: aClass;
		computeEquivalenceClassesForMethodsAndVars;
		computeMessagesSentToVariables;
		computeTypes;
		refineTypesByLookingAtAssignments
]

{ #category : #accessing }
RBRefactoryTyper >> selectedClass: aClass [ 
	class := model classFor: aClass
]

{ #category : #assignments }
RBRefactoryTyper >> typeFor: anObject [ 
	anObject isString
		ifTrue: [ ^ String ].
	anObject isInteger
		ifTrue: [ ^ Integer ].
	^ (anObject == true or: [ anObject == false ])
		ifTrue: [ Boolean ]
		ifFalse: [ anObject class ]
]

{ #category : #accessing }
RBRefactoryTyper >> typesFor: anInstVarName [ 
	^variableTypes at: anInstVarName ifAbsent: [Set new]
]

{ #category : #accessing }
RBRefactoryTyper >> typesFor: anInstVarName in: aClass [ 
	class = aClass ifFalse: [self runOn: aClass].
	^variableTypes at: anInstVarName ifAbsent: [Set new]
]

{ #category : #accessing }
RBRefactoryTyper >> typesFor: variableName in: aParseTree model: aRBSmalltalk [
	| searcher messages |
	searcher := self parseTreeSearcher.
	searcher
		matches: variableName , ' `@message: ``@args'
		do: [ :aNode :answer | 
			answer
				add: aNode selector;
				yourself ].
	messages := searcher executeTree: aParseTree initialAnswer: Set new.
	^ self
		model: aRBSmalltalk;
		findTypeFor: messages; 
		yourself
]
