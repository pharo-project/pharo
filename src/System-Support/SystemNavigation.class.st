"
I support the navigation of the system. 
I act as a facade but as I could require some state or different way of navigating the system all my behavior are on the instance side.
I should (it is not complety done yet) be parametrized by an environment (aSystemDictionary) that scopes my queries.
"
Class {
	#name : #SystemNavigation,
	#superclass : #Object,
	#instVars : [
		'environment'
	],
	#category : #'System-Support-Image'
}

{ #category : #accessing }
SystemNavigation class >> default [
	^ self new
]

{ #category : #query }
SystemNavigation >> allBehaviors [
	^ self environment allBehaviors
]

{ #category : #query }
SystemNavigation >> allBehaviorsDo: aBlock [
	"Execute a block on each class, metaclass, trait and trait class"

	self environment allBehaviorsDo: aBlock
]

{ #category : #query }
SystemNavigation >> allCallsOn: aSymbol [
	"Answer a Collection of all the methods that call on the provided symbol even deeply embedded in literal array."
	"self new allCallsOn: #allCallsOn:"

	^ self allReferencesTo: aSymbol
]

{ #category : #query }
SystemNavigation >> allClassNamesStartingWith: aString do: aBlock [

	self allClassNamesStartingWith: aString do: aBlock caseSensitive: true
]

{ #category : #query }
SystemNavigation >> allClassNamesStartingWith: aString do: aBlock caseSensitive: cs [

	self allClassesDo: [ :e |
		(e name beginsWith: aString caseSensitive: cs)
			ifTrue: [ aBlock value: e name ] ]
]

{ #category : #query }
SystemNavigation >> allClasses [
	"Returns all the classes in the current environment."

	^ self environment allClasses
]

{ #category : #query }
SystemNavigation >> allClassesAndTraits [
	"Answer all the classes and traits present in the image. Note that metaclasses are not part of the answer"

	^ self environment allClassesAndTraits
]

{ #category : #query }
SystemNavigation >> allClassesDo: aBlock [

	^ self environment allClassesDo: aBlock
]

{ #category : #query }
SystemNavigation >> allClassesImplementing: aSelector [
	"Answer all classes that implement the message aSelector."
	^self allBehaviors select: [:class | class includesSelector: aSelector]
]

{ #category : #query }
SystemNavigation >> allExistingProtocolsFor: instanceSide [
	"Answer all protocols on instance or class side"

	^ self allClasses flatCollectAsSet: [ :class |
		  instanceSide
			  ifTrue: [ class protocolNames ]
			  ifFalse: [ class class protocolNames ] ]
]

{ #category : #private }
SystemNavigation >> allGlobalRefsOn: aSymbol [
	"Answer all references to globals"
	"self new allGlobalRefsOn: #ActiveWorld"

	| binding|
	binding := self environment bindingOf: aSymbol.
	binding ifNil: [ ^ #() ].
	^ self allReferencesTo: binding
]

{ #category : #query }
SystemNavigation >> allImplementedMessages [
	"Answer a Collection of all the messages that are implemented in the system."

	^Symbol selectorTable
]

{ #category : #query }
SystemNavigation >> allImplementorsOf: aSelector [
	"Answer all the methods that implement the message aSelector."

	^ self allBehaviors
		select: [ :class | class includesSelector: aSelector ]
		thenCollect: [ :class | class >> aSelector ]
]

{ #category : #query }
SystemNavigation >> allMethods [
	"all methods, including copies from Traits"
	^ self environment allMethods
]

{ #category : #query }
SystemNavigation >> allMethodsSelect: aBlock [
	"Answer a SortedCollection of each method that, when used as the block  argument to aBlock, gives a true result."

	| aCollection |
	aCollection := OrderedCollection new.
	self allBehaviorsDo: [:class |
		class	methodsDo: [:m |
			(aBlock value: m) ifTrue: [aCollection add: m]]].
	^ aCollection
]

{ #category : #query }
SystemNavigation >> allObjects [
	"Answer an Array of all objects in the system.  Fail if there isn't enough memory to instantiate the result."

	<primitive: 178>
	^ self primitiveFailed
]

{ #category : #query }
SystemNavigation >> allObjectsDo: aBlock [
	"Evaluate the argument, aBlock, for each object in the system, excluding immediates such as SmallInteger and Character."

	self allObjectsOrNil
		ifNotNil: [ :allObjects| allObjects do: aBlock ]
		ifNil: [ self error:'Error allocating a big enough array for all objects' ]
]

{ #category : #query }
SystemNavigation >> allObjectsOrNil [
	"Answer an Array of all objects in the system.  Fail if there isn't enough memory to instantiate the result and answer nil."

	<primitive: 178>
	^ nil
]

{ #category : #query }
SystemNavigation >> allPrimitiveMethods [
	"Answer all the methods that are implemented by primitives."

	^self allMethods
		select: [ :method | method isPrimitive ]
		thenCollect: [ :method | method methodClass name , ' ' ,
			     method selector , ' ', method primitive printString ]
]

{ #category : #query }
SystemNavigation >> allReferencesTo: aLiteral [
	"Answer all the methods that refer to aLiteral even deeply embedded in literal array."


	| specialIndex |
	"for speed we check the special selectors here once per class"
	specialIndex := Smalltalk specialSelectorIndexOrNil: aLiteral.
	^ self allBehaviors flatCollect: [ :class | class thoroughWhichMethodsReferTo: aLiteral specialIndex: specialIndex ]
]

{ #category : #query }
SystemNavigation >> allReferencesTo: aLiteral do: aBlock [
	"Perform aBlock on all the references to aLiteral."

	| symbol |
	symbol := aLiteral isSymbol.
	self allBehaviorsDo: [ :class |
		symbol
			ifFalse: [ class withMethodsReferTo: aLiteral do: aBlock ]
			ifTrue: [ class withThorougMethodsReferTo: aLiteral do: aBlock ] ]
]

{ #category : #query }
SystemNavigation >> allReferencesTo: aLiteral in: classes [
	"Answer all the methods that refer to aLiteral even deeply embedded in literal array."

	^ classes flatCollect: [ :class | class thoroughWhichMethodsReferTo: aLiteral]
]

{ #category : #query }
SystemNavigation >> allReferencesToBinding: aVariablesBinding [
	"Answer all the methods that refer to aVariableBinding, do not go into nested Arrays"

	^ self allBehaviors flatCollect: [ :class | class whichMethodsReferTo: aVariablesBinding ]
]

{ #category : #query }
SystemNavigation >> allSelectorsStartingWith: aString do: aBlock [

	Symbol selectorTable do: [ :e |
		(e beginsWith: aString)
			ifTrue: [ aBlock value: e  ] ]
]

{ #category : #'message sends' }
SystemNavigation >> allSendersOf: selector [
	^self allReferencesTo: selector
]

{ #category : #query }
SystemNavigation >> allSentMessages [
	"Answer the set of selectors which are sent somewhere in the system."
	| sent |
	sent := IdentitySet new.
	self allBehaviorsDo: [:each |
				each selectors
					do: [:sel | "Include all sels, but not if sent by self"
						(each compiledMethodAt: sel) literalsDo: [:m |
								(m isSymbol) ifTrue: ["might be sent"
										m == sel ifFalse: [sent add: m]].
								(m isMemberOf: Array) ifTrue: ["might be performed"
										m do: [:x | (x isSymbol) ifTrue: [x == sel ifFalse: [sent add: x]]]]]]].
		"The following may be sent without being in any literal frame"
		1
			to: Smalltalk specialSelectorSize
			do: [:index | sent
					add: (Smalltalk specialSelectorAt: index)].
	^ sent
]

{ #category : #query }
SystemNavigation >> allSentNotImplementedSelectors [

	"Answer all methods where a message is sent but the selector is not implemented anywhere in the system."

	^ self allMethods select: [ :method |
		  | ignored |
		  ignored := method allIgnoredNotImplementedSelectors.

		  method messages anySatisfy: [ :m |
			  m isSelectorSymbol not and: [ (ignored includes: m) not ] ] ]
]

{ #category : #query }
SystemNavigation >> allSymbolsStartingWith: aString do: aBlock [

	Symbol allSymbolTablesDo: [ :e |
		(e beginsWith: aString)
			ifTrue: [ aBlock value: e  ] ]
]

{ #category : #query }
SystemNavigation >> allUnimplementedCalls [
	"call is not a good term here, these are selectors"
	self
		deprecated: 'Changed selector for coherency'
		transformWith: '`@receiver allUnimplementedCalls' -> '`@receiver allSentNotImplementedSelectors'.
	^self allSentNotImplementedSelectors
]

{ #category : #query }
SystemNavigation >> allUnsentMessagesIn: selectorSet [
	"Answer the subset of selectorSet which are not sent anywhere in the system. As allSentMessages does not recurse into nested Arrays, filter with #allCallsOn, which does"

	^ (selectorSet copyWithoutAll: self allSentMessages)
			select: [ :each | (self allCallsOn: each) isEmpty ]
]

{ #category : #accessing }
SystemNavigation >> environment [
	^ environment
]

{ #category : #accessing }
SystemNavigation >> environment: aSystemDictionary [

	environment := aSystemDictionary
]

{ #category : #private }
SystemNavigation >> headingAndAutoselectForLiteral: aLiteral do: binaryBlock [
	"Evaluate aBlock with either Users of ... or Senders of ... plus the auto-select string for the given literal.  aLiteral can be a Symbol, a VariableBinding or an arbitrary object."

	| autoSelect |
	^ aLiteral isSymbol
		ifTrue: [ binaryBlock value: 'Senders of ', aLiteral value: aLiteral ]
		ifFalse:
			[ autoSelect := aLiteral isVariableBinding
							ifTrue: [ aLiteral key ]
							ifFalse: [ aLiteral printString ].
			binaryBlock value: 'Users of ', autoSelect value: autoSelect ]
]

{ #category : #initialization }
SystemNavigation >> initialize [

	super initialize.
	self environment: Smalltalk globals
]

{ #category : #query }
SystemNavigation >> instanceSideMethodsWithNilKeyInLastLiteral [
	"This answers all the instance side methods that has NIL as the key in their last literal. There should be none (only class side methods have this)"

	^ self
		allMethodsSelect: [ :each |
			(((each literalAt: each numLiterals) key isNil
					and: [ (each literalAt: each numLiterals) value isMeta not ])
						and: [ each methodClass ~= UndefinedObject ]) and: [ each isInstalled ] ]
]

{ #category : #'message sends' }
SystemNavigation >> isUnsentMessage: selector [
	^ self allBehaviors
		noneSatisfy: [ :behavior | behavior thoroughHasSelectorReferringTo: selector ]
]

{ #category : #query }
SystemNavigation >> methods [
	"all methods, but without those installed by Traits"
	^ self environment methods
]

{ #category : #'identifying obsoletes' }
SystemNavigation >> methodsReferencingObsoleteClasses [
	"Returns all methods that reference obsolete behaviors"

	| obsClasses |
	obsClasses := self obsoleteClasses.

	^Array streamContents: [ :methods |
		obsClasses keysAndValuesDo:
			[ :index :each | | obsRefs |
			obsRefs := each pointersToExcept: obsClasses.
			obsRefs do: [ :ref |
				"Figure out if it may be a global"
				(ref isVariableBinding and: [ref key isString	"or Symbol"])
					ifTrue: [
						(ref pointersTo) do: [ :meth |
							meth isCompiledMethod
								ifTrue: [methods nextPut: meth]]]]]]
]

{ #category : #query }
SystemNavigation >> methodsWithUnboundGlobals [
	"This says that for any global, it should match either the class's notion of what bindingOf: the key is, or bindingOf: should be nil and the binding should be in Undeclared. If the class answers a different binding through bindingOf: or answers no binding and the binding is not in Undeclared then the variable in the method is wrong.
	For a clean image the result should be empty. If it is not empty, evaluating Compiler recompileAll probably solves the problem. However, we should investigate why the method gets an incorrect state."

	"SystemNavigation new methodsWithUnboundGlobals"

	^self allMethodsSelect:
		[:m|
		m allLiterals anySatisfy:
			[:l|
			l isVariableBinding
			and: [l key isSymbol "avoid class-side methodClass literals"
			and: [ (l isKindOf: AdditionalBinding) not
			and: [(m methodClass classBindingOf: l key)
					ifNil: [(Undeclared associationAt: l key ifAbsent: []) ~~ l]
					ifNotNil: [:b| b ~~ l]]]]]]
]

{ #category : #'identifying obsoletes' }
SystemNavigation >> obsoleteClasses [
	"SystemNavigation new obsoleteClasses inspect"
	"NOTE:  Also try inspecting comments below"

	| obs |
	obs := OrderedCollection new.
	Smalltalk garbageCollect.
	Metaclass
		allInstancesDo: [ :m |
			| c |
			c := m soleInstance.
			(c notNil and: [ c isObsolete ])
				ifTrue: [ obs add: c ] ].
	^ obs asArray

"Likely in a ClassDict or Pool...
(Association allInstances select: [:a | (a value isKindOf: Class) and: [a value isObsolete]]) asArray
"

"Obsolete class refs or super pointer in last lit of a method...
| n l found |
SystemNavigation new browseAllSelect:
	[:m | found := false.
	1 to: m numLiterals do:
		[:i | (((l := m literalAt: i) isMemberOf: Association)
				and: [(l value isKindOf: Behavior)
				and: [l value isObsolete]])
			ifTrue: [found := true]].
	found]
"
]
