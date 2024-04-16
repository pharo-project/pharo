"
I am a refactoring for find occurrences of a method in owner class and in the whole hierarchy if apply.

My precondition verifies that the method exists in specified class, and if occurrences are found in hierarchy this method should not overwritten in hierarchy.

### Example script


```
(RBFindAndReplaceRefactoring 
	find: #methodWithArg:andArg: 
	of: MyClassA 
	inWholeHierarchy: true) execute.
```

Before refactoring:

```
Object << #MyClassA
	package: 'Testing'

MyClassA >> methodWithArg: anArg1 andArg: anArg2
	^ (anArg1 > anArg2) not	

MyClassA << #MyClassB
	package: 'Testing'
	
MyClassB >> someMethod
	^  3
	
MyClassB >> dummyMethod
	(3 > self someMethod) not
```

After refactoring:

```
MyClassB >> dummyMethod 
	self methodWithArg: 3 andArg: self someMethod
```
"
Class {
	#name : 'RBFindAndReplaceTransformation',
	#superclass : 'RBMethodRefactoring',
	#instVars : [
		'method',
		'selector',
		'replacesAllHierarchy',
		'matchNodes',
		'occurrences'
	],
	#category : 'Refactoring-Core-Transformation-Simple',
	#package : 'Refactoring-Core',
	#tag : 'Transformation-Simple'
}

{ #category : 'displaying' }
RBFindAndReplaceTransformation class >> basicMenuItemString [

	^ 'Find and Replace'
]

{ #category : 'instance creation' }
RBFindAndReplaceTransformation class >> find: aMethod of: aClass inWholeHierarchy: aBoolean [

	^ self new
		find: aMethod
		of: aClass
		inWholeHierarchy: aBoolean;
		yourself
]

{ #category : 'instance creation' }
RBFindAndReplaceTransformation class >> isTransformation [ 
	^ true
]

{ #category : 'instance creation' }
RBFindAndReplaceTransformation class >> model: aModel find: aMethod of: aClass inWholeHierarchy: aBoolean [
	^ self new
		model: aModel;
		find: aMethod
		of: aClass
		inWholeHierarchy: aBoolean;
		yourself
]

{ #category : 'preconditions' }
RBFindAndReplaceTransformation >> applicabilityPreconditions [

	| condition rbMethod |
	rbMethod := class methodFor: selector.
	condition := { RBCondition definesSelector: selector in: class }.
	replacesAllHierarchy ifTrue: [
		condition := condition , (class allSubclasses collect: [ :aClass |
			(RBCondition
				 definesSelector: selector
				 in: aClass
				 orIsSimilarTo: rbMethod) not ] ) ].
	^ condition
]

{ #category : 'accessing' }
RBFindAndReplaceTransformation >> argumentsOf: aDictionary [
	"Return the arguments values of a method ocurrence"

	|args limit|
	limit := self method ast arguments size - 1.
	args := OrderedCollection new.
	0 to: limit do: [ :each |
		args add:
			(aDictionary at: (aDictionary keys detect:
				[ :e | (e name asString) =  ('`@argMatch', each asString)])) sourceCode
	 ].
	^ args
]

{ #category : 'accessing' }
RBFindAndReplaceTransformation >> extract: occurrence of: rbMethod [
	[|refactoring |
	refactoring := self extractMethodRefactoring.
	refactoring model: self model.
	refactoring extract: occurrence key from: rbMethod selector in: rbMethod modelClass.
	refactoring setOption: #existingSelector toUse:  [ :ref |
			ref parameters: (self argumentsOf: occurrence value).
			selector].
	self generateChangesFor: refactoring ] on: Exception do: [ :e | e ]
]

{ #category : 'accessing' }
RBFindAndReplaceTransformation >> extractMethodRefactoring [
	^ RBExtractMethodRefactoring new

]

{ #category : 'initialization' }
RBFindAndReplaceTransformation >> find: aSelector of: aClass inWholeHierarchy: aBoolean [

	class := self classObjectFor: aClass.
	selector := aSelector.
	replacesAllHierarchy := aBoolean
]

{ #category : 'accessing' }
RBFindAndReplaceTransformation >> findOccurrencesIn: rbMethod [
	|methodNode sourceCode flag |
	flag := false.
	methodNode := rbMethod ast.
	sourceCode := methodNode sourceCode.
	(self nodesOf: methodNode) do: [ :each |
            each first < each last
                ifTrue: [
	self matchNodes do: [ :matchNode | matchNode
                      match: (self patternParserClass parseExpression: (sourceCode copyFrom: each first to: each last ))
                      onSuccess: [ :map |
	self extract: ((each first to: each last) -> map)
	of: rbMethod.
	occurrences := occurrences + 1.
	flag := true. ]
                      onFailure: [  ] .
						flag ifTrue: [ self findOccurrencesIn: (rbMethod modelClass methodFor: rbMethod selector).
							^ self]]]].
	methodNode body nodesDo: [ :node |
		self matchNodes do: [ :matchNode | matchNode
                      match: node
                      onSuccess: [ :map |
	self extract: ((node start to: node stop) -> map)
	of: rbMethod.
	occurrences := occurrences + 1.
	flag := true.]
                      onFailure: [  ] .
						flag ifTrue: [ self findOccurrencesIn: (rbMethod modelClass methodFor: rbMethod selector).
							^ self]]]
]

{ #category : 'initialization' }
RBFindAndReplaceTransformation >> initialize [
	super initialize.
	occurrences := 0
]

{ #category : 'accessing' }
RBFindAndReplaceTransformation >> matchNodes [

	^ matchNodes ifNil: [
		  | visitor node sourceCode |
		  visitor := RBMatchVisitor new.
		  node := self methodNode.
		  node acceptVisitor: visitor.
		  sourceCode := self replaceArgumentsByPattern: node newSource.
		  sourceCode := sourceCode
			                copyFrom: (self startLimitOf: sourceCode) + visitor difference
			                to: sourceCode size.
		  matchNodes := OrderedCollection new.
		  matchNodes add:
			  (self patternParserClass parseExpression: sourceCode).
		  node lastIsReturn ifTrue: [
			  node hasMultipleReturns ifFalse: [
				  sourceCode := sourceCode copyReplaceAll: '^' with: ''.
				  matchNodes add:
					  (self patternParserClass parseExpression: sourceCode) ] ].
		  matchNodes ]
]

{ #category : 'accessing' }
RBFindAndReplaceTransformation >> method [
	^ method ifNil: [ method := class methodFor: selector ]
]

{ #category : 'accessing' }
RBFindAndReplaceTransformation >> methodNode [

	^ self method ast copy
]

{ #category : 'accessing' }
RBFindAndReplaceTransformation >> nodesOf: methodNode [
	|visitor node|
	visitor := RBCombinatorVisitor new.
	node := methodNode copy.
	node acceptVisitor: visitor.
	^ visitor combinations
]

{ #category : 'accessing' }
RBFindAndReplaceTransformation >> patternParserClass [
	^ RBPatternParser
]

{ #category : 'transforming' }
RBFindAndReplaceTransformation >> privateTransform [
	|classes|
	classes :=replacesAllHierarchy ifFalse: [ { class } ] ifTrue: [ class withAllSubclasses ].
	classes do: [ :cls | (self selectorsFor: cls) do: [ :sel | |rbMethod|
		rbMethod := cls methodFor: sel.
		self findOccurrencesIn: rbMethod] ].
]

{ #category : 'accessing' }
RBFindAndReplaceTransformation >> replaceArgumentsByPattern: sourceCode [
	|newSource|
	newSource := sourceCode copyWithRegex: 'tempMatch*' matchesReplacedWith: '`@tempMatch' .
	newSource := newSource copyWithRegex: 'argMatch*' matchesReplacedWith: '`@argMatch'.
	^ newSource
]

{ #category : 'preconditions' }
RBFindAndReplaceTransformation >> selectorsFor: cls [
	^ cls selectors copyWithout: selector
]

{ #category : 'accessing' }
RBFindAndReplaceTransformation >> startLimitOf: sourceCode [
	^ self method ast body statements first start
]

{ #category : 'storing' }
RBFindAndReplaceTransformation >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' find: #';
		nextPutAll: selector;
		nextPutAll: ' of: #';
		nextPutAll: class name;
		nextPutAll: ' inAllHierarchy: '.
	replacesAllHierarchy storeOn: aStream.
	aStream nextPut: $)
]
