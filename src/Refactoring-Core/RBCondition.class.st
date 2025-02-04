"
I am a refactoring condition for doing a precondition check for refactoring operations.

A precondition check returns true or false and is used by refactoring operations to check whether the operation is applicable to the target entity (class or method refactoring).

You can set the block code used for testing the condition (#withBlock:).
And I define some factory methods on my class side for creating instances of me, for some typically usage. 

For example: 
This creates a condition checking if the class named #Morph implements a selector named #drawOn:
(RBCondition definesSelector:#drawOn: in: (RBClass existingNamed:#Morph)).

Most users of me are refactoring operations and use my methods on the class side for creating instances.

"
Class {
	#name : 'RBCondition',
	#superclass : 'RBAbstractCondition',
	#instVars : [
		'block',
		'errorBlock'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'instance creation' }
RBCondition class >> canUnderstand: aSelector in: aClass [

	^self new
		block: [aClass definesMethod: aSelector]
		errorString: aClass printString , ' <1?:does not >understand<1?s:> ' , aSelector printString
]

{ #category : 'utilities' }
RBCondition class >> checkClassVarName: aName in: aClass [
	| string |
	aName isString ifFalse: [^false].
	string := aName asString.
	(Symbol reservedLiterals includes: string) ifTrue: [^false].
	string isEmpty ifTrue: [^false].
	string first isUppercase ifFalse: [^false].
	^OCScanner isVariable: string
]

{ #category : 'utilities' }
RBCondition class >> checkInstanceVariableName: aName in: aClass [

	| string |
	aName isString ifFalse: [ ^ false ].
	string := aName.
	string ifEmpty: [ ^ false ].
	(Symbol reservedLiterals includes: string) ifTrue: [ ^ false ].
	string first isUppercase ifTrue: [ ^ false ].
	^ OCScanner isVariable: string
]

{ #category : 'instance creation' }
RBCondition class >> definesClassVariable: aString in: aClass [
	^self new
		block: [aClass definesClassVariable: aString]
		errorString: aClass printString
				, ' <1?:does not >define<1?s:> class variable ' , aString
]

{ #category : 'instance creation' }
RBCondition class >> definesInstanceVariable: aString in: aClass [
	^self new
		block: [aClass definesInstanceVariable: aString]
		errorString: aClass printString
				, ' <1?:does not >define<1?s:> instance variable ' , aString
]

{ #category : 'instance creation' }
RBCondition class >> definesSelector: aSelector in: aClass [
	^self new
		block: [aClass directlyDefinesMethod: aSelector]
		errorString: aClass printString , ' <1?:does not >define<1?s:> ' , aSelector printString
]

{ #category : 'instance creation' }
RBCondition class >> definesSelector: aSelector in: aClass orIsSimilarTo: rbMethod [
	^self new
		block: [(aClass directlyDefinesMethod: aSelector)
			ifTrue: [ (aClass parseTreeForSelector: aSelector) ~= rbMethod parseTree ]
			ifFalse: [ false ]
			]
		errorString: aClass printString , ' <1?:does not >define<1?s:> ' , aSelector printString
]

{ #category : 'instance creation' }
RBCondition class >> definesTempVar: aString in: aClass ignoreClass: subclass [

	| condition |
	condition := self new.
	condition
		block: [
			| method |
			method := self
				          methodDefiningTemporary: aString
				          in: aClass
				          ignore: [ :class :aSelector |
				          class includesClass: subclass ].
			method ifNotNil: [
				condition errorMacro:
					method printString , ' defines variable ' , aString ].
			method isNotNil ]
		errorString:
			aClass printString
			, ' <1?:does not >define<1?s:> temporary variable ' , aString.
	^ condition
]

{ #category : 'instance creation' }
RBCondition class >> definesTemporaryVariable: aString in: aClass [

	| condition |
	condition := self new.
	condition
		block: [
			| method |
			method := self
				          methodDefiningTemporary: aString
				          in: aClass
				          ignore: [ :class :selector | false ].
			method ifNotNil: [
				condition errorMacro:
					method printString , ' defines variable ' , aString ].
			method isNotNil ]
		errorString:
			aClass printString
			, ' <1?:does not >define<1?s:> temporary variable ' , aString.
	^ condition
]

{ #category : 'instance creation' }
RBCondition class >> directlyDefinesClassVariable: aString in: aClass [
	^self new
		block: [aClass directlyDefinesClassVariable: aString]
		errorString: aClass printString
				, ' <1?:does not >directly define<1?s:> class variable ' , aString
]

{ #category : 'instance creation' }
RBCondition class >> directlyDefinesInstanceVariable: aString in: aClass [
	^self new
		block: [aClass directlyDefinesInstanceVariable: aString]
		errorString: aClass printString
				, ' <1?:does not >directly define<1?s:> instance variable ' , aString
]

{ #category : 'instance creation' }
RBCondition class >> empty [
	"Returns an empty condition"
	self deprecated: 'Use RBCondition true instead' transformWith: '`@rec empty' -> '`@ true'.
	^ self true
]

{ #category : 'instance creation' }
RBCondition class >> hasSuperclass: aClass [
	^self new
		block: [aClass superclass isNil not]
		errorString: aClass printString , ' has <1?a:no> superclass'
]

{ #category : 'instance creation' }
RBCondition class >> hierarchyOf: aClass canUnderstand: aSelector [
	^self new
		block: [aClass hierarchyDefinesMethod: aSelector]
		errorString: aClass printString , ' <1? and/or part of it''s Hierarchy already: and/or part of it''s Hierarchy do not> understand<1?s:> ' , aSelector printString
]

{ #category : 'instance creation' }
RBCondition class >> hierarchyOf: aClass definesVariable: aString [
	^self new
		block: [aClass hierarchyDefinesVariable: aString]
		errorString: aClass printString
				, ' or one of its subclasses <1?:does not >define<1?s:> variable '
					, aString
]

{ #category : 'instance creation' }
RBCondition class >> hierarchyOf: aClass referencesInstanceVariable: aString [

	^ self new
		  block: [
			  aClass withAllSubclasses anySatisfy: [ :each |
				  (each whichSelectorsReferToInstanceVariable: aString) isNotEmpty ] ]
		  errorString: aClass printString
			  , ' or subclass <1?:does not >reference<1?s:> instance variable '
			  , '#' , aString
]

{ #category : 'instance creation' }
RBCondition class >> hierarchyOf: aClass referencesSharedVariable: aString [
	"Returns whether a shared variable is accessed anywhere in a class hierarchy, being it class or instance side. "
	
	^ self new
		  block: [
			| aclass |
			aclass := aClass instanceSide.
			  (aclass withAllSubclasses anySatisfy: [ :each |
				   (each whichSelectorsReferToClassVariable: aString) isNotEmpty ])
				  or: [
					  aclass classSide withAllSubclasses anySatisfy: [ :each |
						  (each whichSelectorsReferToClassVariable: aString) isNotEmpty ] ] ]
		  errorString: aClass printString
			  , ' or subclass <1?:does not >reference<1?s:> shared variable '
			  , '#' , aString
]

{ #category : 'utilities' }
RBCondition class >> invalidArgumentNamesForSelector: aSymbol in: aModel [
	"Return the list of elements that when added as argument to a selector would break it e.g., an argument cannot have the same name as a instance variables or the same name as an already existing argument."
	
	| invalidArgNames |
	invalidArgNames := Set new.
	(aModel allImplementorsOf: aSymbol) do: [ :e | 
		invalidArgNames 
			addAll: e instanceVariableNames;
			addAll: (e methodFor: aSymbol) argumentNames.
		 ].
	^ invalidArgNames
]

{ #category : 'instance creation' }
RBCondition class >> isAbstractClass: aClass [

	^ self new
		  block: [ aClass isAbstract ]
		  errorString: aClass printString , ' is <1?:not >an abstract class'
]

{ #category : 'instance creation' }
RBCondition class >> isClass: anObject [
	^self new
		block: [anObject isBehavior]
		errorString: anObject printString , ' is <1?:not >a behavior'
]

{ #category : 'testing' }
RBCondition class >> isClass: aRBClass definedIn: aRBNamespace [ 
	^ self isClassNamed: aRBClass name definedIn: aRBNamespace 
]

{ #category : 'instance creation' }
RBCondition class >> isClassNamed: className definedIn: aModel [

	^ self new 
		block: [ | aClassOrTrait |
				aClassOrTrait := aModel classNamed: className.
				aClassOrTrait isNotNil ]
		errorString: [ className , ' is <1?:not > defined' ]
]

{ #category : 'instance creation' }
RBCondition class >> isGlobal: aString in: aRBSmalltalk [

	^ self new block: [ aRBSmalltalk includesGlobal: aString asSymbol ] errorString: aString , ' is <1?:not >a class or global variable'
]

{ #category : 'instance creation' }
RBCondition class >> isImmediateSubclass: subclass of: superClass [

	^ self new
		  block: [ subclass superclass = superClass ]
		  errorString:
			  subclass printString , ' is <1?:not >an immediate subclass of '
			  , superClass printString
]

{ #category : 'instance creation' }
RBCondition class >> isMetaclass: anObject [

	^ self new
		  block: [ anObject isMeta ]
		  errorString: anObject printString , ' is <1?:not >a metaclass'
]

{ #category : 'instance creation' }
RBCondition class >> isSubclass: subclass of: superClass [

	^ self new
		  block: [ subclass includesClass: superClass ]
		  errorString: subclass printString , ' is <1?:not >a subclass of '
			  , superClass printString
]

{ #category : 'instance creation' }
RBCondition class >> isSymbol: aString [

	^ self new
		  block: [ aString isSymbol ]
		  errorString: aString , ' is <1?:not >a symbol'
]

{ #category : 'instance creation' }
RBCondition class >> isValidClassName: aString [

	^ self new
		  block: [ self checkClassVarName: aString in: self ]
		  errorString: aString , ' is <1?:not >a valid class name'
]

{ #category : 'instance creation' }
RBCondition class >> isValidClassVarName: aString for: aClass [

	^ self new
		  block: [ self checkClassVarName: aString in: aClass ]
		  errorString: aString , ' is <1?:not >a valid class variable name'
]

{ #category : 'instance creation' }
RBCondition class >> isValidInstanceVariableName: aString for: aClass [

	^ self new
		  block: [ self checkInstanceVariableName: aString in: aClass ]
		  errorString:
		  aString , ' is <1?:not >a valid instance variable name'
]

{ #category : 'testing' }
RBCondition class >> isValidSelector: aString [
	"Returns a Condition that checks that aString is a valid selector (name of method)."
	
	"Pay attention this class side method returns an RBCondition object,
	while Refactoring>> isValidSelector: retuns a boolean. 
	In the future we may want to have two different names for this two methods."
	
	^ self new
		  block: [ OCScanner isSelector: aString ]
		  errorString:
		  aString printString , ' is <1?:not >a valid method name'
]

{ #category : 'utilities' }
RBCondition class >> methodDefiningTemporary: aString in: aClass ignore: aBlock [

	| searcher method |
	searcher := OCParseTreeSearcher new.
	method := nil. "Shut-up the warning"
	searcher matches: aString do: [ :aNode :answer | ^ method ].
	aClass withAllSubclasses do: [ :class |
		class selectors do: [ :each |
			(aBlock value: class value: each) ifFalse: [
				| parseTree |
				method := class methodFor: each.
				parseTree := class parseTreeForSelector: each.
				parseTree ifNotNil: [ searcher executeTree: parseTree ] ] ] ].
	^ nil
]

{ #category : 'instance creation' }
RBCondition class >> referencesClassVariable: aString in: aClass [

	^ self new
		  block: [
		  	(aClass whichSelectorsReferToClassVariable: aString) isNotEmpty and: 
			[ (aClass classSide whichSelectorsReferToClassVariable: aString) isNotEmpty ] ]
		  errorString:
			  aClass printString
			  , ' <1?:does not >reference<1?s:> class variable ' , aString
]

{ #category : 'instance creation' }
RBCondition class >> referencesInstanceVariable: aString in: aClass [

	^ self new
		  block: [
		  (aClass whichSelectorsReferToInstanceVariable: aString) isNotEmpty ]
		  errorString: aClass printString
			  , ' <1?:does not >reference<1?s:> instance variable ' , aString
]

{ #category : 'instance creation' }
RBCondition class >> subclassesOf: aClass isDoingASuperSendFor: aSelector [

	^ self new
		  block: [
			  aClass subclasses anySatisfy: [ :each |
				  each selectors anySatisfy: [ :sel |
					  | tree |
					  tree := each parseTreeForSelector: sel.
					  tree isNotNil and: [ tree superMessages includes: aSelector ] ] ] ]
		  errorString:
			  '<1?:no:a> subclass of ' , aClass printString , ' is doing a super send using '
			  , aSelector printString
]

{ #category : 'instance creation' }
RBCondition class >> true [
	"Returns a true condition. It is useful when chaining condition. It acts as neutral element of the operation such AND."

	^self new
		block: [true]
		errorString: 'true'
]

{ #category : 'instance creation' }
RBCondition class >> withBlock: aBlock [
	^self new withBlock: aBlock
]

{ #category : 'instance creation' }
RBCondition class >> withBlock: aBlock errorString: aString [

	^ self new block: aBlock errorString: aString
]

{ #category : 'initialization' }
RBCondition >> block: aBlock errorString: aString [

	block := aBlock.
	self errorMacro: aString
]

{ #category : 'checking' }
RBCondition >> check [
	^block value
]

{ #category : 'accessing' }
RBCondition >> errorBlock [

	^ errorBlock
]

{ #category : 'initialization' }
RBCondition >> errorBlock: anObject [
	errorBlock := anObject
]

{ #category : 'displaying' }
RBCondition >> violationMessageOn: aWriteStream [ 

	aWriteStream nextPutAll: self errorString
]

{ #category : 'initialization' }
RBCondition >> withBlock: aBlock [
	block := aBlock
]
