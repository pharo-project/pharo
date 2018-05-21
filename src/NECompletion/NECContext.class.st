"
A completion is started by the ECController. The controller creates me to compute the context of the completion. The most important information about the context are the receiverClass and the completionToken. I create a ECModel or subclass when requested by the 'model' method.

I use SHParser and SHRange to parse the text input.
"
Class {
	#name : #NECContext,
	#superclass : #Object,
	#instVars : [
		'source',
		'position',
		'theClass',
		'ranges',
		'completionIndex',
		'recurseCount',
		'receiverClass',
		'completionToken',
		'model',
		'controller',
		'variables',
		'selectors'
	],
	#category : #'NECompletion-Model'
}

{ #category : #'instance creation' }
NECContext class >> controller: aECController class: aClass source: aString position: anInteger [ 
	^ self basicNew initialize
		setController: aECController
		class: aClass
		source: aString
		position: anInteger
]

{ #category : #'private-temporaries' }
NECContext >> blockTemporaries [
	| blocks range vars |
	blocks := OrderedCollection new.
	1 
		to: completionIndex - 1
		do: 
			[ :index | 
			range := ranges at: index.
			self 
				handleBlockStack: blocks
				with: range.
			range isBlockTemporary ifTrue: 
				[ vars := blocks last.
				vars add: range ] ].
	^ self convertBlocksToVariables: blocks
]

{ #category : #'private-compute-index' }
NECContext >> checkForZeroPosition [
	^ (position = 0
			or: [ranges isEmpty])
		and: [self insertEmptyRangeAt: 1 start: 0 end: 1.
			true]
]

{ #category : #'private-receiver-guessing' }
NECContext >> checkImpossibleReceiver [
	^ self isSelectorsAndVariables
		ifTrue: [self configureSelectorsAndVariables]
		ifFalse: [self isVariablesOnly
				ifTrue: [self configureVariablesOnly]
				ifFalse: [self isSelectorsOnly
						and: [self configureSelectorsOnly]]]. 
]

{ #category : #accessing }
NECContext >> completionToken [
	completionToken
		ifNil:
			[ self isPositionAtPatternArg
				ifTrue: [ completionToken := self tokenForMethodDefinition ]
				ifFalse: [ completionToken := self tokenAtCurrentRange ] ].
	^ completionToken
]

{ #category : #'private-type-guessing' }
NECContext >> completionTokenIsComment [
	| currentRange |
	currentRange := (ranges at: completionIndex).
	^  currentRange isComment
]

{ #category : #'private-type-guessing' }
NECContext >> completionTokenIsLiteral [
	| currentRange |
	currentRange := (ranges at: completionIndex).
	^  currentRange isConstant or:[ currentRange isUnfinished ]
]

{ #category : #'private-type-guessing' }
NECContext >> completionTokenIsLiteralOrComment [
	^ self completionTokenIsLiteral or:[ self completionTokenIsComment]
]

{ #category : #private }
NECContext >> compute [
	completionIndex := self computeIndexOfPosition.
	receiverClass := self computeReceiverClass
]

{ #category : #'private-compute-index' }
NECContext >> computeIndexOfPosition [
	| current |
	self checkForZeroPosition ifTrue: [ ^ 1 ].
	1 
		to: ranges size
		do: 
			[ :index | 
			current := ranges at: index.
			(current includesPosition: position) 
				ifTrue: [ ^ index ]
				ifFalse: 
					[ current end > position ifTrue: [ ^ self createEmptyRangeForGapAt: index ] ] ].
	^ self createEmptyRangeAtTail
]

{ #category : #private }
NECContext >> computeReceiverClass [
	| previous found |
	recurseCount := 0.
	completionIndex = 1 ifTrue: [^nil].
	found := self checkImpossibleReceiver.
	found ifTrue: [^nil].
	previous := ranges at: completionIndex - 1.
	previous type = #cascadeSeparator 
		ifTrue: [^self guessCascadeReceiver: completionIndex - 1].
	(previous type = #unary 
		and: [(self sourceOf: previous) = 'new' and: [completionIndex > 2]]) 
			ifTrue: 
				[previous := ranges at: completionIndex - 2.
				previous type = #globalVar 
					ifTrue: [^self guessTypeOf: completionIndex - 2]
					ifFalse: 
						[self configureSelectorsOnly.
						^nil]].
	^self guessTypeOf: completionIndex - 1
]

{ #category : #'private-configure' }
NECContext >> configureSelectorsAndVariables [
	variables := true.
	selectors := true.
	^true
]

{ #category : #'private-configure' }
NECContext >> configureSelectorsOnly [
	variables := false.
	selectors := true.
	^true
]

{ #category : #'private-configure' }
NECContext >> configureVariablesOnly [
	variables := true.
	selectors := false.
	^true
]

{ #category : #'private-temporaries' }
NECContext >> convertBlocksToVariables: anOrderedCollection [ 
	| result blockStack |
	blockStack := anOrderedCollection.
	result := OrderedCollection new.
	blockStack do: [ :each | result addAll: each ].
	^ result
]

{ #category : #'private-compute-index' }
NECContext >> createEmptyRangeAtTail [
	| previous |
	previous := ranges last.
	ranges add: (SHRange 
			start: previous end + 1
			end: source size
			type: #empty).
	^ ranges size
]

{ #category : #'private-compute-index' }
NECContext >> createEmptyRangeForGapAt: index [ 
	| current previous |
	current := ranges at: index.
	previous := ranges at: (index - 1 max: 1).
	self 
		insertEmptyRangeAt: index
		start: previous end + 1
		end: current start - 1.
	^ index
]

{ #category : #accessing }
NECContext >> createModel [
	| modelClass |
	
	self receiverClass
		ifNotNil: [^ NECTypedModel class: receiverClass].

	(controller workspace isNil and: [ theClass notNil ])
		ifTrue: [ self isStartOfMethodDefinition
				ifTrue: [ NECPreferences overrideModel
						ifTrue: [ ^ NECOverrideModel class: theClass ]
						ifFalse: [ ^ NECEmptyModel new ] ] ].
							
	self completionTokenIsLiteralOrComment ifTrue:[ ^ NECEmptyModel new].
				
	modelClass := NECUntypedModel.		
	(controller model respondsTo: #modelClass) 
		ifTrue: [
			controller model modelClass ifNotNil: [:class | modelClass :=  class]].	
			
	^ modelClass
		class: theClass
		temporaries: self temporaries
		additionals: controller additionals
		variables: variables
		selectors: selectors
]

{ #category : #private }
NECContext >> createRanges [
	| parser |
	parser := SHParserST80 new.
	ranges := parser 
				rangesIn: source
				classOrMetaClass: theClass
				workspace: controller workspace
				environment: nil.
	"filter out all comments, they don't have any completion/type related information, except the
	one for the current position, as this is used to prevent completion within comments"
	ranges := ranges select: [ :each | each type ~= #comment or: [ each includesPosition: position ] ]
]

{ #category : #private }
NECContext >> currentRange [
	^ ranges detect: [ :each | position between: each start and: each end ] ifNone: [ nil ]
]

{ #category : #private }
NECContext >> findSourceRangeFor: aNumber [ 
	aNumber
		to: ranges size
		by: 2
		do: [:index | 
			index + 3 > ranges size
				ifTrue: [^ nil].
			(ranges at: index + 1) isAssignment
				ifTrue: [(ranges at: index + 3) type = #statementSeparator
						ifTrue: [^ index + 2]
						ifFalse: [(ranges at: index + 2) type = #globalVar
								ifTrue: [^ index + 2]
								ifFalse: [(ranges at: index + 3) isAssignment 
										ifFalse: [^ nil]]]]]
]

{ #category : #'private-type-guessing' }
NECContext >> guessArgument: aSHRange [ 
	| name |
	name := self sourceOf: aSHRange.
	(name = 'html') ifTrue: [
		 Smalltalk at: #WAHtmlCanvas ifPresent: [ :canvasClass | ^canvasClass].
		 Smalltalk at: #WARenderCanvas ifPresent: [ :canvasClass | ^canvasClass]].
	^ NECInstVarTypeGuesser getClassFromTypeSuggestingName: name
]

{ #category : #private }
NECContext >> guessCascadeReceiver: aNumber [ 
	| type |
	aNumber 
		to: 1
		by: -1
		do: 
			[ :index | 
			type := (ranges at: index) type.
			((#(#statementSeparator #assignment #ansiAssignment ) includes: type) or: [ (type beginsWith: 'pattern') or: [ type beginsWith: 'methodTemp' ] ]) ifTrue: [ ^ self guessTypeOf: index + 1 ] ].
	^ nil
]

{ #category : #private }
NECContext >> guessClassVarClass: aSHRange [ 
	| aClass name |
	name := self sourceOf: aSHRange.
	aClass := controller guessTypeFor: name.
	aClass
		ifNotNil: [^ aClass].
	^ (NECClassVarTypeGuesser 
		variableName: name 
		class: theClass classSide) perform
]

{ #category : #'private-type-guessing' }
NECContext >> guessGlobal: aNumber [ 
	| aClass |
	aClass := Smalltalk 
		at: (self sourceOf: (ranges at: aNumber)) asSymbol
		ifAbsent: [ ^ nil ].
	
	aNumber = (completionIndex - 1) ifTrue: [ ^ aClass class ].
	
	aClass isBehavior ifFalse: [ ^ nil ].
	
	(ranges size >= (aNumber + 1) and: [ 
		(ranges at: aNumber + 1) type = #statementSeparator ]) ifTrue: [ ^ aClass class ].
	
	^ aClass
]

{ #category : #private }
NECContext >> guessInstVarClass: aSHRange [ 
	| aClass name |
	name := self sourceOf: aSHRange.
	aClass := controller guessTypeFor: name.
	aClass ifNotNil: [ ^ aClass ].
	^ (NECInstVarTypeGuesser 
		variableName: name
		class: theClass) perform
]

{ #category : #private }
NECContext >> guessTempVarClass: aSHRange [ 
	^self guessTempVarClass: (self sourceOf: aSHRange) type: aSHRange type.

]

{ #category : #private }
NECContext >> guessTempVarClass: aString type: aSymbol [ 
	| current type varName varType sourceIndex aClass |
	aClass := controller guessTypeFor: aString.
	aClass ifNotNil: [ ^ aClass ].
	varName := aString.
	varType := aSymbol.
	1 
		to: completionIndex
		do: 
			[ :index | 
			current := ranges at: index.
			(current type = varType and: [ (self sourceOf: current) = varName and: [ index + 3 <= ranges size ] ]) ifTrue: 
				[ (sourceIndex := self findSourceRangeFor: index) notNil ifTrue: 
					[ type := self guessTypeOf: sourceIndex.
					type ifNotNil: [ ^ type ] ] ] ].
	^ nil
]

{ #category : #'private-type-guessing' }
NECContext >> guessTypeOf: aNumber [ 
	| range |
	self configureSelectorsOnly.
	recurseCount > 10 ifTrue: [ ^ nil ].
	recurseCount := recurseCount + 1.
	range := ranges at: aNumber.
	
	range isSelf ifTrue: [ ^theClass ].
	(range isSuper and:[ theClass notNil]) ifTrue: [ ^theClass superclass ].
	range isConstant ifTrue: [ ^self nonAbstractTypeOf: range ].
	range isArgument ifTrue: [ ^self guessArgument: range ].
	range isTemporaryVariable ifTrue: [ ^self guessTempVarClass: range ].
	range isInstanceVariable ifTrue: [ ^self guessInstVarClass: range ].
	range isClassVariable ifTrue: [ ^self guessClassVarClass: range ].
	range isGlobal ifTrue: [ ^self guessGlobal: aNumber ].
	
	^nil
]

{ #category : #'private-temporaries' }
NECContext >> handleBlockStack: aCollection with: aSHRange [ 
	| range blockStack |
	range := aSHRange.
	blockStack := aCollection.
	range isBlockStart 
		ifTrue: [ blockStack add: OrderedCollection new ]
		ifFalse: [ range isBlockEnd ifTrue: [ blockStack removeLast ] ]
]

{ #category : #initialization }
NECContext >> initialize [
	super initialize.
	source := String new.
	position := 0.
	recurseCount := 0.
	variables := true.
	selectors := true.
	ranges := OrderedCollection new.
	completionIndex := 0.
	completionToken := nil
]

{ #category : #'private-compute-index' }
NECContext >> insertEmptyRangeAt: index start: start end: end [ 
	ranges
		add: (SHRange
				start: start
				end: end
				type: #empty)
		beforeIndex: index
]

{ #category : #private }
NECContext >> isPositionAtPatternArg [
	| currentRange |
	currentRange := self currentRange.
	^ currentRange notNil and: [ currentRange type = #patternArg ]
]

{ #category : #'private-receiver-guessing' }
NECContext >> isSelectorsAndVariables [
	| current |
	current := ranges at: completionIndex.
	^current isUnfinished
]

{ #category : #'private-receiver-guessing' }
NECContext >> isSelectorsOnly [
	| previous |
	previous := ranges at: completionIndex - 1.
	^previous isOpening
]

{ #category : #private }
NECContext >> isStartOfMethodDefinition [
	^ completionIndex = 1 or: [ self isPositionAtPatternArg ]
]

{ #category : #'private-receiver-guessing' }
NECContext >> isVariablesOnly [
	| current previous |
	current := ranges at: completionIndex.
	^ current isVariablesOnly or: 
		[ current isOpening or: 
			[ previous := ranges at: completionIndex - 1.
			previous isOpening or: 
				[ previous isSeparator or: 
					[ previous isKeyword or: [ previous isAssignment or: [ previous isBinary ] ] ] ] ] ]
]

{ #category : #accessing }
NECContext >> model [
	model isNil ifTrue: [ model := self createModel ].
	^ model
]

{ #category : #action }
NECContext >> narrowWith: aString [ 
	completionToken := aString.
	model ifNotNil: [ model narrowWith: aString ]
]

{ #category : #'private-type-guessing' }
NECContext >> nonAbstractTypeOf: aRange [
	| cls |
	cls := aRange asType.
	cls hasAbstractMethods
		ifFalse: [ ^ cls ].
	^ [(cls readFromString: (aRange textIn: source)) class] on: Error do: [ cls ]
]

{ #category : #private }
NECContext >> receiverClass [
	^ receiverClass
]

{ #category : #'initialize-release' }
NECContext >> setController: aECController class: aClass source: aString position: anInteger [ 
	controller := aECController.
	theClass := aClass.
	source := aString.
	position := anInteger.
	self createRanges.
	self compute
]

{ #category : #private }
NECContext >> sourceOf: aSHRange [ 
	^aSHRange isString
		ifTrue: [aSHRange]
		ifFalse: [self sourceOf: aSHRange stopAt: aSHRange end]
]

{ #category : #private }
NECContext >> sourceOf: aSHRange stopAt: aNumber [
	^ aSHRange type = #empty
		ifTrue: [String new]
		ifFalse: [source copyFrom: aSHRange start to: aNumber]
]

{ #category : #action }
NECContext >> switchToUntyped [
	receiverClass := nil.
	self configureSelectorsAndVariables.
	model := self createModel
]

{ #category : #accessing }
NECContext >> temporaries [
	| tempRanges |
	tempRanges := ranges select: [ :each | #(#patternTempVar #patternArg ) includes: each type ].
	tempRanges addAll: self blockTemporaries.
	^ tempRanges collect: 
		[ :each | 
		NECLocalEntry 
			contents: (self sourceOf: each)
			type: each type ]
]

{ #category : #accessing }
NECContext >> theClass [
	^theClass
]

{ #category : #private }
NECContext >> tokenAtCurrentRange [
	| range token |
	range := ranges at: completionIndex.
	token := self sourceOf: range stopAt: position.
	^ token wordBefore: token size
]

{ #category : #private }
NECContext >> tokenForMethodDefinition [
	^ self sourceOf: ranges first stopAt: position
]
