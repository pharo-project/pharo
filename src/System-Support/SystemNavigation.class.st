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
	#category : #'System-Support'
}

{ #category : #accessing }
SystemNavigation class >> default [
	^ self new
]

{ #category : #query }
SystemNavigation >> allAccessesTo: instVarName from: aClass [
	| coll |
	coll := OrderedCollection new.
	aClass
		withAllSubAndSuperclassesDo: [ :class | (class whichSelectorsAccess: instVarName) do: [ :sel | coll add: (self createMethodNamed: sel realParent: class) ] ].
	^ coll
]

{ #category : #query }
SystemNavigation >> allBehaviors [
	^self environment allBehaviors
]

{ #category : #query }
SystemNavigation >> allBehaviorsDo: aBlock [
	"Execute a block on each class, metaclass, trait and trait class"

	self environment allClassesDo: [ :aClass | 
		aBlock
			value: aClass;
			value: aClass theMetaClass ].

	self environment allTraitsDo: [ :aTrait | 
		aBlock
			value: aTrait;
			value: aTrait theMetaClass ]
]

{ #category : #query }
SystemNavigation >> allCallsOn: aSymbol [ 
	"Answer a Collection of all the methods that call on the provided symbol even deeply embedded in literal array."
	"self new allCallsOn: #allCallsOn:"
	
	^ self allReferencesTo: aSymbol
]

{ #category : #query }
SystemNavigation >> allCallsOn: aSymbol from: aClass [
	"Answer a SortedCollection of all the methods that call on aSymbol."

	| collection |
	collection := OrderedCollection  new.
	aClass withAllSubclassesDo: [ :class |
		(class whichSelectorsReferTo: aSymbol) do: [:sel |
				collection add: (self createMethodNamed: sel realParent: class)]].
	^collection
]

{ #category : #query }
SystemNavigation >> allClasses [
	"currently returns all the classes in the current environment"

	^ self environment allClasses

	
]

{ #category : #query }
SystemNavigation >> allClassesAndTraits [
	"Answer all the classes and traits present in the image"
	"Note that metaclasses are not part of the answer"
	
	^ self environment allClassesAndTraits
]

{ #category : #query }
SystemNavigation >> allClassesDo: aBlock [

	^ self environment allClassesDo: aBlock

	
]

{ #category : #query }
SystemNavigation >> allClassesImplementing: aSelector [  
	"Answer an Array of all classes that implement the message aSelector."

	| aCollection |
	aCollection := ReadWriteStream on: Array new.
	self allBehaviorsDo: [:class | (class includesSelector: aSelector) ifTrue: [aCollection nextPut: class]].
	^ aCollection contents
]

{ #category : #packages }
SystemNavigation >> allClassesInPackageNamed: packageName [
	"Returns the list of classes that belongs to a Monticello package."
	
	"It would be probably better to delegate that query to the package itself"
	| classes organization |
	organization := self environment organization.
	classes := (organization classesInCategory: packageName) asSet.
	(organization categoriesMatching: packageName, '-*') do: [:category| 
		classes addAll: (organization classesInCategory: category)].
	^ classes asArray
]

{ #category : #query }
SystemNavigation >> allClassesUsingSharedPool: aString [  
	"Answer an Array of all classes that uses the shared pool named aString."

	| aCollection |
	aCollection := ReadWriteStream on: Array new.
	self allClassesDo: [:class | (class includesSharedPoolNamed: aString) ifTrue: [aCollection nextPut: class]].
	^ aCollection contents
]

{ #category : #query }
SystemNavigation >> allExistingProtocolsFor: instanceSide [

	"answer all protocols on instance or class side"
	
	| allExistingProtocols |
	allExistingProtocols := Smalltalk allClassesAndTraits
		inject: IdentitySet new
		into: [ :col :e | 
			| class |
			class := instanceSide
				ifTrue: [ e theNonMetaClass ]
				ifFalse: [ e theMetaClass ].
			col
				addAll: class protocols;
				yourself ].
	^ allExistingProtocols
]

{ #category : #private }
SystemNavigation >> allGlobalRefsOn: aSymbol [

	| binding|
	
	binding := self environment bindingOf: aSymbol .
	binding ifNil: [ ^ #() ].
	
	^ self allReferencesTo: binding	
	
]

{ #category : #query }
SystemNavigation >> allImplementedMessages [
	"Answer a Collection of all the messages that are implemented in the system."
	| messages |
	messages := IdentitySet new.
	self allBehaviorsDo: [:each | messages addAll: each selectors].
	^ messages
]

{ #category : #query }
SystemNavigation >> allImplementorsOf: aSelector [ 
	"Answer all the methods that implement the message aSelector."
	
	| aCollection |
	aCollection := OrderedCollection  new.
	self allBehaviorsDo: [:class | 
		(class includesSelector: aSelector)
			ifTrue: [aCollection add: (self createMethodNamed: aSelector realParent: class)]].
	^ aCollection
]

{ #category : #query }
SystemNavigation >> allLocalCallsOn: aSymbol ofClass: aClass [
	"Answer a SortedCollection of all the methods that call on aSymbol, anywhere in my class hierarchy."
	
	| aSet cls |
	aSet := Set new.
	cls := aClass theNonMetaClass.
	cls withAllSuperAndSubclassesDo: [ :class |
		(class whichSelectorsReferTo: aSymbol)
			do: [:sel |
				aSet add: (self createMethodNamed: sel realParent: class) ]].
		
	cls class withAllSuperAndSubclassesDo: [ :class |
		(class whichSelectorsReferTo: aSymbol)
			do: [:sel |
				aSet add: (self createMethodNamed: sel realParent: class) ]].
	^aSet
]

{ #category : #query }
SystemNavigation >> allMethods [
	^self allBehaviors flatCollect: [ :behavior | behavior methods]

]

{ #category : #query }
SystemNavigation >> allMethodsSelect: aBlock [ 
	"Answer a SortedCollection of each method that, when used as the block  
	argument to aBlock, gives a true result."
	| aCollection |
	aCollection := OrderedCollection new.
	self allBehaviorsDo: [:class | class
						selectorsAndMethodsDo: [:sel :m | (aBlock value: m)
								ifTrue: [aCollection
										add: (self createMethodNamed: sel realParent: class)]]].
	^ aCollection
]

{ #category : #query }
SystemNavigation >> allMethodsWithSourceString: aString matchCase: caseSensitive [
	"Answer a SortedCollection of all the methods that contain, in source code, aString as a substring.  Search the class comments also"

	| list addMethod addComment |
	list := OrderedCollection new.
	
	addMethod := [ :mrClass :mrSel | list add: (self createMethodNamed: mrSel realParent:  mrClass)].									
	addComment := [ :mrClass | list add: (RGCommentDefinition realClass: mrClass)].
										
	'Searching all source code...'
		displayProgressFrom: 0 to: (2*self environment classAndTraitNames size)
		during: [:bar | | count |
			count := 0.
			self allBehaviorsDo: [:each |
				bar current: (count := count + 1).
					each selectorsDo: [:sel | 
						((each sourceCodeAt: sel) includesSubstring: aString caseSensitive: caseSensitive) ifTrue: [
								 addMethod value: each value: sel]].
					(each organization classComment asString includesSubstring: aString caseSensitive: caseSensitive) ifTrue: [
								addComment value: each]	]].
	^ list 
]

{ #category : #query }
SystemNavigation >> allObjects [
	"Answer an Array of all objects in the system.  Fail if
	 there isn't enough memory to instantiate the result."
	<primitive: 178>
	^self primitiveFailed
]

{ #category : #query }
SystemNavigation >> allObjectsDo: aBlock [ 
	"Evaluate the argument, aBlock, for each object in the system, excluding immediates
	 such as SmallInteger and Character."
	self allObjectsOrNil
		ifNotNil: [:allObjects| allObjects do: aBlock]
		ifNil:[self error:'Error allocating a big enough array for all objects']
]

{ #category : #query }
SystemNavigation >> allObjectsOrNil [
	"Answer an Array of all objects in the system.  Fail if there isn't
	 enough memory to instantiate the result and answer nil."
	<primitive: 178>
	^nil
]

{ #category : #query }
SystemNavigation >> allPrimitiveMethods [
	"Answer an OrderedCollection of all the methods that are implemented by primitives."
	| aColl |
	aColl := OrderedCollection new: 200.
	self allBehaviorsDo: [:class | class
		selectorsAndMethodsDo: [:sel :method | 
			method isPrimitive
				ifTrue: [aColl addLast: class name , ' ' , sel , ' ' , method primitive printString]]].
	^ aColl
]

{ #category : #query }
SystemNavigation >> allReferencesTo: aLiteral [ 
	"Answer a Collection of all the methods that refer to aLiteral even deeply embedded in literal array."

	"SystemNavigation new allReferencesTo: #+"
	"SystemNavigation new allReferencesTo: Point binding"
	
	| result |
	result := OrderedCollection new.

	self allBehaviorsDo: [ :class |
		| selectors | 
		selectors :=  class thoroughWhichSelectorsReferTo: aLiteral.
		selectors do: [ :selector |
			result addLast: (self createMethodNamed: selector realParent: class) ] ].
	^ result
]

{ #category : #query }
SystemNavigation >> allReferencesToPool: aPool [
	"Answer all the references to variable aPool"

	| list |
	list := OrderedCollection new.
	self
		allClassesDo: [ :cls | 
			cls
				selectorsAndMethodsDo: [ :sel :meth | 
					meth literals
						detect: [ :lit | (lit isVariableBinding and: [ lit key notNil ]) and: [ (aPool bindingOf: lit key) notNil ] ]
						ifFound: [ list add: (self createMethodNamed: sel realParent: cls) ] ] ].
	^ list
]

{ #category : #query }
SystemNavigation >> allReferencesToPool: aPool from: aClass [
	"Answer all the references to variables from aPool"

	| list |
	list := OrderedCollection new.
	aClass
		withAllSubclassesDo: [ :cls | 
			cls
				selectorsAndMethodsDo: [ :sel :meth | 
					meth literals
						detect: [ :lit | lit isVariableBinding and: [ (aPool bindingOf: lit key) notNil ] ]
						ifFound: [ list add: (self createMethodNamed: sel realParent: aClass) ] ] ].
	^ list
]

{ #category : #'message sends' }
SystemNavigation >> allSendersOf: selector [ 
	|  senders |
	senders := OrderedCollection new.
	self allBehaviorsDo: [ :behavior | 
		(behavior thoroughWhichSelectorsReferTo: selector) do: [ :sel| 
			senders add: (self createMethodNamed: sel realParent: behavior) ] ].
	^ senders
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
SystemNavigation >> allStoresInto: instVarName of: aClass [
    | coll |
    coll := OrderedCollection new.
    aClass
        withAllSubAndSuperclassesDo: [ :class | (class whichSelectorsStoreInto: instVarName) do: [ :sel | coll add: (self createMethodNamed: sel realParent: class) ] ].
    ^ coll
]

{ #category : #query }
SystemNavigation >> allUnimplementedCalls [
	"Answer an Array of each message that is sent by an expression in a  
	method but is not implemented by any object in the system."
	| aStream all |
	all := self allImplementedMessages.
	aStream := (Array new: 50) writeStream.
	self
		allBehaviorsDo: [:cl | cl
				selectorsAndMethodsDo: [:sel :meth |
					| secondStream ignored | 
					secondStream := (String new: 5) writeStream.
					ignored := (meth pragmaAt: #ignoreUnimplementedCalls:) 
						ifNotNil: [ :pragma | (pragma argumentAt: 1)  ] 
						ifNil: [ #() ].
					meth messages
						do: [:m | ((all includes: m) not and: [ (ignored includes: m) not])
								ifTrue: [ secondStream nextPutAll: m; space ]].
					secondStream position = 0
						ifFalse: [aStream nextPut: cl name , ' ' , sel , ' calls: ' , secondStream contents]]].
	^ aStream contents
]

{ #category : #query }
SystemNavigation >> allUnreferencedClassVariablesOf: aClass [
	"Answer a list of the names of all the receiver's unreferenced class  
	vars, including those defined in superclasses"
	| aList |
	aList := OrderedCollection new.
	aClass withAllSuperclasses
		reverseDo: [:aSuperClass | aSuperClass classVarNames
				do: [:var | (self allReferencesTo: (aSuperClass classPool associationAt: var)) isEmpty
						ifTrue: [aList add: var]]].
	^ aList
]

{ #category : #query }
SystemNavigation >> allUnsentMessagesIn: selectorSet [ 
	"Answer the subset of selectorSet which are not sent anywhere in the 
	system. As allSentMessages does not recurse into nested Arrays, filter
	with #allCallsOn, which does"
	
	^ (selectorSet copyWithoutAll: self allSentMessages) select: [:each | (self allCallsOn: each) isEmpty]   
]

{ #category : #packages }
SystemNavigation >> categoriesInPackageNamed: packageName [
	^(self environment organization categoriesMatching: packageName), (self environment organization categoriesMatching: (packageName, '-*'))
]

{ #category : #query }
SystemNavigation >> createMethodNamed: aSelector realParent: class [

	"This method has some problems."
	"1 - It should use the object that comes from the browsed environment"
	"2 - It does not create a method but a representation of it so be careful"
	^ (RGMethodDefinition realClass: class selector: aSelector) package:  (class>>aSelector) package asRingDefinition; yourself .
]

{ #category : #accessing }
SystemNavigation >> environment [
	^environment 
]

{ #category : #accessing }
SystemNavigation >> environment: aSystemDictionary [
	^environment :=  aSystemDictionary
]

{ #category : #private }
SystemNavigation >> headingAndAutoselectForLiteral: aLiteral do: binaryBlock [
	"Evaluate aBlock with either Users of ... or Senders of ... plus the auto-select string
	 for the given literal.  aLiteral can be a Symbol, a VariableBinding or an arbitrary object."

	| autoSelect |
	^aLiteral isSymbol
		ifTrue: [binaryBlock value: 'Senders of ', aLiteral value: aLiteral ]
		ifFalse:
			[autoSelect := aLiteral isVariableBinding
							ifTrue: [aLiteral key]
							ifFalse: [aLiteral printString].
			binaryBlock value: 'Users of ', autoSelect value: autoSelect]
]

{ #category : #initialization }
SystemNavigation >> initialize [

	super initialize.
	self environment: Smalltalk globals
]

{ #category : #query }
SystemNavigation >> installedMethodsWithIncorrectTrailerKind [

	^self allMethodsSelect:
		[:each | (#(#SourcePointer #VarLengthSourcePointer #NativeCodeTrailer) includes: each trailer kind) not and: [ each isInstalled ]  ]
]

{ #category : #query }
SystemNavigation >> instanceSideMethodsWithNilKeyInLastLiteral [
	"This answers all the instance side methods that has NIL as the key in their last literal. There should be none (only class side methods have this)"

	^ self
		allMethodsSelect: [ :each | 
			(((each literalAt: each numLiterals) key isNil and: [ (each literalAt: each numLiterals) value isMeta not ])
				and: [ each methodClass ~= UndefinedObject ]) and: [ each isInstalled ] ]
]

{ #category : #'message sends' }
SystemNavigation >> isUnsentMessage: selector [

	self allBehaviorsDo: [ :behavior | 
		(behavior thoroughHasSelectorReferringTo: selector) ifTrue: [^false] ].
	^true
]

{ #category : #'message sends' }
SystemNavigation >> isUsedClass: aClass [

	self allBehaviorsDo: [ :behavior | 
		(behavior hasSelectorReferringTo: aClass binding) ifTrue: [^true] ].
	^false
]

{ #category : #query }
SystemNavigation >> methodsWithUnboundGlobals [
	"This says that for any global, it should match either the class's notion of what bindingOf: the key is, or bindingOf: should be nil and the binding should be in Undeclared. If the class answers a different binding through bindingOf: or answers no binding and the binding is not in Undeclared then the variable in the method is wrong.
	For a clean image the result should be empty. If it is not empty, evaluating Compiler recompileAll probably solves the problem. However, we should investigate why the method gets an incorrect state."

	"SystemNavigation new methodsWithUnboundGlobals"

	^self allMethodsSelect:
		[:m|
		m literals anySatisfy:
			[:l|
			l isVariableBinding
			and: [l key isSymbol "avoid class-side methodClass literals"
			and: [ (l value isKindOf: Slot) not
			and: [(m methodClass classBindingOf: l key)
					ifNil: [(Undeclared associationAt: l key ifAbsent: []) ~~ l]
					ifNotNil: [:b| b ~~ l]]]]]]
]

{ #category : #query }
SystemNavigation >> obsoleteBehaviors [
	"SystemNavigation new obsoleteBehaviors inspect"
	"Find all obsolete behaviors including meta classes"

	| obs |
	obs := OrderedCollection new.
	Smalltalk garbageCollect.
	self 
		allObjectsDo: [:cl | (cl isBehavior
					and: [cl isObsolete])
				ifTrue: [obs add: cl]].
	^ obs asArray
]

{ #category : #query }
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
			(c notNil and: [ 'AnOb*' match: c name asString ])
				ifTrue: [ obs add: c ] ].
	^ obs asArray	"Likely in a ClassDict or Pool...
(Association allInstances select: [:a | (a value isKindOf: Class) and: ['AnOb*' match: a value name]]) asArray
"	"Obsolete class refs or super pointer in last lit of a method...
| n l found |
Smalltalk browseAllSelect:
	[:m | found := false.
	1 to: m numLiterals do:
		[:i | (((l := m literalAt: i) isMemberOf: Association)
				and: [(l value isKindOf: Behavior)
				and: ['AnOb*' match: l value name]])
			ifTrue: [found := true]].
	found]
"
]

{ #category : #query }
SystemNavigation >> obsoleteMethodReferences [
	"SystemNavigation new obsoleteMethodReferences"

	"Open a browser on all referenced behaviors that are obsolete"

	| obsClasses references |
	references := Array new writeStream.
	obsClasses := self obsoleteBehaviors.
	'Scanning for methods referencing obsolete classes' 
		displayProgressFrom: 0 to: obsClasses size
		during: 
			[ :bar | 
			obsClasses keysAndValuesDo: 
					[ :index :each | | obsRefs | 
					bar current: index.
					obsRefs := each pointersToExcept: obsClasses.
					obsRefs do: 
							[ :ref | 
							"Figure out if it may be a global"
							(ref isVariableBinding and: [ref key isString	"or Symbol"]) 
								ifTrue: 
									[(ref pointersTo) do: 
											[:meth | 
											meth isCompiledMethod
												ifTrue: [meth methodReference ifNotNil: [:mref | references nextPut: mref]]]]]]].
	^ references contents
]

{ #category : #packages }
SystemNavigation >> removeAllButPackages: packagesList [
  "Remove all classes and methods that do not belong to some of listed packages. Skip #unload. Do not use this method if you can use Monticello! This method is used by some low-level shrinking process. Do not use it. This is quite hackish"

  | keep remove oldRemove extensionCategories |
  keep := Set new.      
  packagesList do: [:pkg | keep addAll: (self allClassesInPackageNamed: pkg)].
  oldRemove := Set new.
  [ 
    remove := self environment allClassesAndTraits reject: [:c | keep includes: c].
    remove := remove collect: [:c | c name].
    (remove asSortedCollection: [:a :b | a  <= b ]) do: [:className |
      | class |
      class := (self environment classOrTraitNamed: className).
      class allSubclasses ifEmpty: [
        SystemAnnouncer uniqueInstance unsubscribe: class.
        class isTrait ifFalse: [ 
          (class class includesSelector: #unload) ifTrue: [ class removeSelector: #unload ] ].
        class removeFromSystem. 
        ] ].

    
    oldRemove ifNotEmpty: [
      oldRemove = remove ifTrue: [          
         self error: 'Some classes still have subclasses: ', (remove collect: [:c | c -> (self environment classOrTraitNamed: c) allSubclasses]) asString ] ].
    oldRemove := remove.
    remove notEmpty] whileTrue.

  keep do: [: class | 
    extensionCategories := (class organization ownCategories select: [:category | 
      category first = $*]) asOrderedCollection.
    extensionCategories removeAllSuchThat: [:category |
      packagesList anySatisfy: [:package |
        ((category asLowercase allButFirst = package asLowercase) or: [package asLowercase, '-*' match: category asLowercase allButFirst])]].
    extensionCategories do: [:category | 
      | protocol |
      protocol := class organization  protocolOrganizer protocolNamed: category.
      (protocol methods intersection: class localSelectors) do: [:sel | class removeSelector: sel ].
      "class removeProtocol: category" ]. 
    class isTrait ifFalse: [
      extensionCategories := (class class organization ownCategories select: [:category | 
        category first = $*]) asOrderedCollection.
      extensionCategories removeAllSuchThat: [:category |
        packagesList anySatisfy: [:package |
          ((category asLowercase allButFirst = package asLowercase) or: [package asLowercase, '-*' match: category asLowercase allButFirst])]].
      extensionCategories do: [:category | 
        | protocol |
        protocol := class class organization  protocolOrganizer protocolNamed: category.
        (protocol methods intersection: class class localSelectors) do: [:sel | class class removeSelector: sel ].
        "class class removeProtocol: category" ]]].
      

]
