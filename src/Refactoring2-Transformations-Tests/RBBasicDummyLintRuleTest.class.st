Class {
	#name : #RBBasicDummyLintRuleTest,
	#superclass : #RBDummyLintRuleTest,
	#instVars : [
		'methodBlock',
		'result',
		'classBlock',
		'anInstVar'
	],
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #'possible bugs' }
RBBasicDummyLintRuleTest class >> abstractClass [
	| detector subclassResponsibilitySymbol |
	detector := self new.
	detector name: 'References an abstract class'.
	detector resultClass: RBClassEnvironment.
	subclassResponsibilitySymbol := 'subclassResponsibility' asSymbol.
	detector classBlock: 
			[:context :result | 
			(context selectedClass whichSelectorsReferTo: subclassResponsibilitySymbol)
				ifNotEmpty: 
						[(context whichSelectorsReferTo: subclassResponsibilitySymbol) ifNotEmpty:  [result addClass: context selectedClass]]].
	^detector
]

{ #category : #'possible bugs' }
RBBasicDummyLintRuleTest class >> addRemoveDependents [
	| detector |
	detector := self new.
	detector resultClass: RBClassEnvironment.
	detector name: 'Number of addDependent: messages > removeDependent:'.
	detector classBlock: 
			[:context :result | 
			| count |
			count := 0.
			((Set
				withAll: (context selectedClass whichSelectorsReferTo: #addDependent:))
					addAll: (context selectedClass whichSelectorsReferTo: #removeDependent:);
				yourself) do: 
					[:sel | 
					(context selectedClass compiledMethodAt: sel) messages do: 
							[:each | 
							each == #addDependent: ifTrue: [count := count + 1].
							each == #removeDependent: ifTrue: [count := count - 1]]].
			count > 0 ifTrue: [result addClass: context selectedClass]].
	^detector
]

{ #category : #miscellaneous }
RBBasicDummyLintRuleTest class >> assignmentInBlock [
	^self createParseTreeRule: #(
			'`@cursor showWhile: [| `@temps | `@.Statements1. `var := `@object]' 
			'`@cursor showWhile: [| `@temps | `@.Statements1. ^`@object]' 
			'[| `@temps | `@.Statements. `var := `@object] ensure: `@block' 
			'[| `@temps | `@.Statements. ^`@object] ensure: `@block' 
			'[| `@temps | `@.Statements. `var := `@object] ifCurtailed: `@block' 
			'[| `@temps | `@.Statements. ^`@object] ifCurtailed: `@block' ) 
		name: 'Unnecessary assignment or return in block'
]

{ #category : #'intention revealing' }
RBBasicDummyLintRuleTest class >> assignmentInIfTrue [
	^self createParseTreeRule: 
			#('`@boolean 
					ifTrue: [| `@temps1 | `@.Statements1. `var := `@object1] 
					ifFalse: [| `@temps2 | `@.Statements2. `var := `@object2]' 
			'`@boolean 
					ifFalse: [| `@temps1 | `@.Statements1. `var := `@object1] 
					ifTrue: [| `@temps2 | `@.Statements2. `var := `@object2]') 
		name: 'Assignment to same variable and end of ifTrue:ifFalse: blocks'
]

{ #category : #'intention revealing' }
RBBasicDummyLintRuleTest class >> atIfAbsent [
	^self createParseTreeRule: 
				#('`@object 
						at: `@atArg 
						ifAbsent: [| `@temps | `@.Statements. `@object at: `@atArg put: `@putArg]' 
				'`@object 
						at: `@atArg 
						ifAbsent: [| `@temps | 
								`@.Statements. 
								`@object at: `@atArg put: `@putArg. 
								`@.xStatements1. 
								`@putArg]') 
		name: 'Uses at:ifAbsent: instead of at:ifAbsentPut:'
]

{ #category : #bugs }
RBBasicDummyLintRuleTest class >> booleanPrecedence [
	^self createParseTreeRule: 
			#('`@object1 | `@object2 = `@object3'
			'`@object1 | `@object2 == `@object3'
			'`@object1 & `@object2 = `@object3'
			'`@object1 & `@object2 == `@object3'
			'`@object1 | `@object2 ~= `@object3'
			'`@object1 | `@object2 ~~ `@object3'
			'`@object1 & `@object2 ~= `@object3'
			'`@object1 & `@object2 ~~ `@object3')
		name: 'Uses A | B = C instead of A | (B = C)'
]

{ #category : #private }
RBBasicDummyLintRuleTest class >> canCall: aSelector in: aClass from: anApplication [
	"This method contains on purpose not implemented messages, such as rootApplication "
	| methodApp root |
	(aClass canUnderstand: aSelector) ifFalse: [^false].
	root := anApplication rootApplication.
	methodApp := ((aClass whichClassIncludesSelector: aSelector) compiledMethodAt: aSelector)
				application rootApplication.
	^methodApp == root or: [root isBasedOn: methodApp]
]

{ #category : #miscellaneous }
RBBasicDummyLintRuleTest class >> classNameInSelector [
	| detector |
	detector := self new.
	detector name: 'Redundant class name in selector'.
	detector methodBlock: 
			[:context :result | 
			(context selectedClass isMetaclass and: 
					[(context selector
						indexOfSubCollection: context selectedClass theNonMetaClass name
						startingAt: 1) > 0])
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #'unnecessary code' }
RBBasicDummyLintRuleTest class >> classNotReferenced [
	| detector |
	detector := self new.
	detector name: 'Class not referenced'.
	detector resultClass: RBClassEnvironment.
	detector classBlock: 
			[:context :result | 
			(context selectedClass isMetaclass
				or: [context isApplication
				or: [context selectedClass subclasses isEmpty not]])
					ifFalse: 
						[ context selectedClass allCallsOn ifEmpty:
								[result addClass: context selectedClass;
									addClass: context selectedClass class]]].
	^detector
]

{ #category : #private }
RBBasicDummyLintRuleTest class >> classShouldNotOverride [
	^#(#== #class)
]

{ #category : #'possible bugs' }
RBBasicDummyLintRuleTest class >> collectionCopyEmpty [
	| detector |
	detector := self new.
	detector
		name: 'Subclass of collection that has instance variable but doesn''t define copyEmpty'.
	detector resultClass: RBClassEnvironment.
	detector classBlock: 
			[:context :result | 
			(context selectedClass isVariable 
					and: [(context selectedClass includesSelector: #copyEmpty:) not 
						and: [context selectedClass instVarNames isEmpty not
							and: [context selectedClass inheritsFrom: Collection]]])
				ifTrue: [result addClass: context selectedClass]].
	^detector
]

{ #category : #'intention revealing' }
RBBasicDummyLintRuleTest class >> collectionMessagesToExternalObject [
	| detector matcher |
	detector := self new.
	detector name: 'Sends add:/remove: to external collection'.
	matcher := RBParseTreeSearcher new.
	matcher
		addSearches: (#(#add: #remove: #addAll: #removeAll:)
				collect: [:each | ('(`@Object `@message: `@args) <1s> `@Arg' expandMacrosWith: each) asString])
					-> 
						[:aNode :answer | 
						answer or: 
								[(aNode receiver selector copyFrom: 1 to: (aNode receiver selector size min: 2))
									~= 'as' and: 
											[| receiver |
											receiver := aNode receiver receiver.
											receiver isVariable not or: 
													[((#('self' 'super') includes: receiver name)
														or: [Smalltalk includesKey: receiver name asSymbol]) not]]]].
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #'intention revealing' }
RBBasicDummyLintRuleTest class >> collectionProtocol [
	^self createParseTreeRule: 
			#('`@collection do: [:`each | | `@temps | `@.Statements1. `@object add: `@arg. `@.Statements2]' 
			'`@collection do: [:`each | | `@temps | 
					`@.Statements1. 
					`@condition ifTrue: [| `@blockTemps | 
							`@.BlockStatements1. 
							`@object add: `each. 
							`@.BlockStatements2]. 
					`@.Statements2]' 
			'`@collection do: [:`each | | `@temps | 
					`@.Statements1. 
					`@condition ifFalse: [| `@blockTemps | 
							`@.BlockStatements1. 
							`@object add: `each. 
							`@.BlockStatements2]. 
					`@.Statements2]') 
		name: 'Uses do: instead of collect: or select:''s'
]

{ #category : #'intention revealing' }
RBBasicDummyLintRuleTest class >> consistencyCheck [
	^self createParseTreeRule: 
				#('`@object size == 0' 
				'`@object size = 0' 
				'`@object size > 0' 
				'`@object size >= 1' 
				'`@object == nil' 
				'`@object = nil') 
		name: 'Uses "size = 0" or "= nil" instead of "isEmpty" or "isNil"'
]

{ #category : #'intention revealing' }
RBBasicDummyLintRuleTest class >> contains [
	^self createParseTreeRule: 
			#('(`@object detect: [:`each | | `@temps| `@.Statements] ifNone: [nil]) isNil' 
			'(`@object detect: [:`each | | `@temps| `@.Statements] ifNone: [nil]) notNil' 
			'(`@object detect: [:`each | | `@temps| `@.Statements] ifNone: [nil]) = nil' 
			'(`@object detect: [:`each | | `@temps| `@.Statements] ifNone: [nil]) == nil' 
			'(`@object detect: [:`each | | `@temps| `@.Statements] ifNone: [nil]) ~= nil' 
			'(`@object detect: [:`each | | `@temps| `@.Statements] ifNone: [nil]) ~~ nil' 
			'`@object detect: [:`each | | `@temps| `@.Statements] ifNone: [| `@temps1 | `@.Statements2. ^`@anything]') 
		name: 'Uses detect:ifNone: instead of contains:'
]

{ #category : #private }
RBBasicDummyLintRuleTest class >> createMatcherFor: codeStrings method: aBoolean [ 
	| matcher |
	matcher := RBParseTreeSearcher new.
	aBoolean
		ifTrue: [matcher addMethodSearches: codeStrings -> [:aNode :answer | true]]
		ifFalse: [matcher addSearches: codeStrings -> [:aNode :answer | true]].
	^matcher
]

{ #category : #'instance creation' }
RBBasicDummyLintRuleTest class >> createParseTreeRule: codeStrings method: aBoolean name: aName [ 
	| detector matcher |
	detector := self new.
	detector name: aName.
	matcher := self createMatcherFor: codeStrings method: aBoolean.
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #'instance creation' }
RBBasicDummyLintRuleTest class >> createParseTreeRule: codeStrings name: aName [ 
	^self createParseTreeRule: codeStrings
		method: false
		name: aName
]

{ #category : #'possible bugs' }
RBBasicDummyLintRuleTest class >> definesEqualNotHash [
	| detector |
	detector := self new.
	detector name: 'Defines = but not hash'.
	detector resultClass: RBClassEnvironment.
	detector classBlock: 
			[:context :result | 
			((context selectedClass includesSelector: #=)
				and: [(context selectedClass includesSelector: #hash) not])
				ifTrue: [result addClass: context selectedClass]].
	^detector
]

{ #category : #'intention revealing' }
RBBasicDummyLintRuleTest class >> detectContains [
	^self createParseTreeRule: 
			#('`@collection do: [:`each | | `@temps | 
					`@.Statements1. 
					`@condition ifFalse: [| `@BlockTemps | `@.BlockStatements1. ^`each]. 
					`@.Statements2]' 
			'`@collection do: [:`each | | `@temps | 
					`@.Statements1. 
					`@condition ifTrue: [| `@BlockTemps | `@.BlockStatements1.  ^`each]. 
					`@.Statements2]' 
			'`@collection do: [:`each | | `@temps | 
					`@.Statements1. 
					`@condition ifFalse: [| `@BlockTemps | `@.BlockStatements1. ^true]. 
					`@.Statements2]' 
			'`@Collection do: [:`each | | `@temps | 
					`@.Statements1. 
					`@condition ifTrue: [| `@BlockTemps | `@.BlockStatements1.  ^true]. 
					`@.Statements2]' 
			'`@collection do: [:`each | | `@temps | 
					`@.Statements1. 
					`@condition ifFalse: [| `@BlockTemps | `@.BlockStatements1. ^false]. 
					`@.Statements2]' 
			'`@collection do: [:`each | | `@temps | 
					`@.Statements1. 
					`@condition ifTrue: [| `@BlockTemps | `@.BlockStatements1.  ^false]. 
					`@.Statements2]') 
		name: 'Uses do: instead of contains: or detect:''s'
]

{ #category : #'unnecessary code' }
RBBasicDummyLintRuleTest class >> endTrueFalse [
	| detector matcher |
	detector := self new.
	detector name: 'Check for same statements at end of ifTrue:ifFalse: blocks'.
	matcher := (RBParseTreeSearcher new) addSearches: 
				#('`@object 
						ifTrue: [| `@temps1 | `@.Statements1. `.Statement] 
						ifFalse: [| `@temps2 | `@.Statements2. `.Statement]' 
				'`@object 
						ifTrue: [| `@temps1 | `.Statement. `@.Statements1] 
						ifFalse: [| `@temps2 | `.Statement. `@.Statements2]' 
				'`@object 
						ifFalse: [| `@temps1 | `@.Statements1. `.Statement] 
						ifTrue: [| `@temps2 | `@.Statements2. `.Statement]' 
				'`@object 
						ifFalse: [| `@temps1 | `.Statement. `@.Statements1] 
						ifTrue: [| `@temps2 | `.Statement. `@.Statement2]') 
			-> [:aNode :answer | answer
						or: 
							[| node |
							node := aNode arguments first body statements last.
							(node isVariable and: [node = aNode arguments last body statements last]) not]]; yourself.
	detector methodBlock: [:context :result | (matcher executeTree: context parseTree initialAnswer: false)
			ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #'unnecessary code' }
RBBasicDummyLintRuleTest class >> equalsTrue [
	| detector matcher |
	detector := self new.
	detector name: 'Unnecessary "= true"'.
	matcher := (RBParseTreeSearcher new) addSearches: #('true' 'false') -> 
								[:aNode :answer | 
								answer or: 
										[aNode parent isMessage
											and: [#(#= #== #~= #~~) includes: aNode parent selector]]];
				yourself.
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #'unnecessary code' }
RBBasicDummyLintRuleTest class >> equivalentSuperclassMethods [
	| detector |
	detector := self new.
	detector name: 'Methods equivalently defined in superclass'.
	detector methodBlock: 
			[:context :result | 
			context selectedClass superclass notNil ifTrue: 
					[(context selectedClass superclass canUnderstand: context selector)
						ifTrue: 
							[(((context selectedClass superclass
								whichClassIncludesSelector: context selector)
									compiledMethodAt: context selector)
									equivalentTo: context compiledMethod)
									ifTrue: [result addClass: context selectedClass selector: context selector]]]].
	^detector
]

{ #category : #'unnecessary code' }
RBBasicDummyLintRuleTest class >> extraBlock [
	^self
		createParseTreeRule: (#('value' 'value: `@value' 'value: `@value1 value: `@value2' 'value: `@value1 value: `value2 value: `@value3' 'valueWithArguments: `@values')
				collect: [:each | '[:`@params | | `@temps | `@.statements] ' , each])
		name: 'Block immediately evaluated'
]

{ #category : #'possible bugs' }
RBBasicDummyLintRuleTest class >> fileBlocks [
	^self createParseTreeRule: 
				#('[| `@temps | 
					`var := `@object.  
					`@.statements] 
							ensure: 
								[`var `@messages: `@args]' 
				'[| `@temps | 
					`var := `@object.  
					`@.statements] 
							ifCurtailed: 
								[`var `@messages: `@args]') 
		name: 'Assignment inside unwind blocks should be outside.'
]

{ #category : #miscellaneous }
RBBasicDummyLintRuleTest class >> fullBlocks [

"skip this test in squeak."

"	| detector |
	detector := self new.
	detector name: 'Method with full blocks'.
	detector methodBlock: 
			[:context :result | 
			context compiledMethod withAllBlockMethodsDo: 
					[:method | 
					method needsHybridFrame
						ifTrue: [result addClass: context selectedClass selector: context selector]]].
	^detector"
]

{ #category : #'intention revealing' }
RBBasicDummyLintRuleTest class >> guardingClause [
	^self
		createParseTreeRule: 
				#('`@MethodName: `@args 
						| `@temps | 
						`@.Statements. 
						`@condition ifTrue: [| `@BlockTemps | `.Statement1. `.Statement2. `@.BStatements]' 
				'`@MethodName: `@args 
						| `@temps | 
						`@.Statements. 
						`@condition ifFalse: [| `@BlockTemps | `.Statement1. `.Statement2. `@.BStatements]')
		method: true
		name: 'Guarding clauses'
]

{ #category : #miscellaneous }
RBBasicDummyLintRuleTest class >> ifTrueBlocks [
	| detector matcher |
	detector := self new.
	detector name: 'Non-blocks in ifTrue:/ifFalse: messages'.
	matcher := RBParseTreeSearcher new.
	matcher
		addSearches: #('``@condition ifTrue: ``@block' '``@condition ifFalse: ``@block' '``@condition ifTrue: ``@block1 ifFalse: ``@block2' '``@condition ifFalse: ``@block1 ifTrue: ``@block2')
				-> 
					[:aNode :answer | 
					answer or: 
							[(aNode arguments detect: [:each | each isBlock not] ifNone: [nil]) notNil]].
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #'intention revealing' }
RBBasicDummyLintRuleTest class >> ifTrueReturns [
	| detector matcher |
	detector := self new.
	detector name: 'ifTrue:/ifFalse: returns instead of and:/or:''s'.
	matcher := RBParseTreeSearcher new.
	matcher addSearches:
		#('| `@temps | ``@.Statements. ``@object ifTrue: [^``@value1]. ^``@value2' 
		'| `@temps | ``@.Statements. ``@object ifFalse: [^``@value1]. ^``@value2') 
			-> [:aNode :answer | answer
				or: 
					[| node |
					node := (aNode statements at: aNode statements size - 1) arguments first body statements last value. "``@value1"
					(node isLiteralNode and: [{true. false} includes: node value])
						or: 
							[node := aNode statements last value.
							node isLiteralNode and: [{true. false} includes: node value]]]].
	detector methodBlock: [:context :result | (matcher executeTree: context parseTree initialAnswer: false)
			ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #'unnecessary code' }
RBBasicDummyLintRuleTest class >> implementedNotSent [
	| detector |
	detector := self new.
	detector name: 'Methods implemented but not sent'.
	detector methodBlock: 
			[:context :result | 
			context compiledMethod senders
				senders: [result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #miscellaneous }
RBBasicDummyLintRuleTest class >> instVarInSubclasses [
	| detector |
	detector := self new.
	detector name: 'Instance variables defined in all subclasses'.
	detector result: nil pullUpInstVar.
	detector classBlock: 
			[:context :result | 
			| subs |
			subs := context selectedClass subclasses.
			subs size > 1 ifTrue: 
					[| sels |
					sels := Bag new.
					subs do: [:each | sels addAll: each instVarNames].
					sels asSet do: 
							[:val | 
							(sels occurrencesOf: val) == subs size
								ifTrue: [result addInstVar: val for: context selectedClass]]]].
	^detector
]

{ #category : #'unnecessary code' }
RBBasicDummyLintRuleTest class >> justSendsSuper [
	| detector matcher |
	detector := self new.
	detector name: 'Method just sends super message'.
	matcher := RBParseTreeSearcher justSendsSuper.
	detector methodBlock: 
			[:context :result | 
			(context parseTree tag isNil
				and: [matcher executeMethod: context parseTree initialAnswer: false])
					ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #private }
RBBasicDummyLintRuleTest class >> longMethodSize [
	^10
]

{ #category : #miscellaneous }
RBBasicDummyLintRuleTest class >> longMethods [
	| detector matcher |
	detector := self new.
	detector name: 'Long methods'.
	matcher := RBParseTreeSearcher new.
	matcher
		addSearch: '`.Stmt' -> 
					[:aNode :answer | 
					(aNode children inject: answer
						into: [:sum :each | matcher executeTree: each initialAnswer: sum]) + 1].
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: 0)
				>= self longMethodSize
					ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #private }
RBBasicDummyLintRuleTest class >> metaclassShouldNotOverride [
	^#(#name #comment)
]

{ #category : #'intention revealing' }
RBBasicDummyLintRuleTest class >> minMax [
	| detector matcher |
	detector := self new.
	detector name: 'Uses ifTrue:/ifFalse: instead of min: or max:'.
	matcher := RBParseTreeSearcher new.
	matcher
		addSearches: #('(`x `message: `@y) `ifTrue: [`x := `@y]' '(`@x `message: `@y) `ifTrue: [`@x] `ifFalse: [`@y]')
				-> 
					[:aNode :answer | 
					answer or: 
							[(#(#ifTrue: #ifFalse: #ifTrue:ifFalse: #ifFalse:ifTrue:)
								includes: aNode selector)
									and: [#(#< #<= #> #>=) includes: aNode receiver selector]]].
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #'intention revealing' }
RBBasicDummyLintRuleTest class >> missingSubclassResponsibility [
	| detector |
	detector := self new.
	detector name: 'Method defined in all subclasses, but not in superclass'.
	"detector resultClass: MultiEnvironment."
	detector classBlock: 
			[:context :result | 
			| subs |
			subs := context selectedClass subclasses.
			subs size > 1 & context selectedClass isMetaclass not ifTrue: 
					[| sels |
					sels := Bag new.
					subs do: [:each | sels addAll: each selectors].
					sels asSet do: 
							[:each | 
							((sels occurrencesOf: each) == subs size
								and: [(context selectedClass canUnderstand: each) not]) ifTrue: 
										[| envName |
										envName := context selectedClass name , '>>', each.
										subs do: 
												[:subClass | 
												result addClass: subClass
													selector: each
													into: envName]]]]].
	^detector
]

{ #category : #'possible bugs' }
RBBasicDummyLintRuleTest class >> missingYourself [
	| detector matcher |
	detector := self new.
	detector name: 'Possible missing "; yourself"'.
	matcher := RBParseTreeSearcher new.
	matcher
		addSearch: '``@xobject `@messages: ``@args' -> 
					[:aNode :answer | 
					answer or: 
							[aNode parent isCascade
								and: [aNode isDirectlyUsed and: [aNode selector ~~ #yourself]]]].
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #'possible bugs' }
RBBasicDummyLintRuleTest class >> modifiesCollection [
	| detector addSearcher |
	detector := self new.
	detector name: 'Modifies collection while iterating over it'.
	addSearcher := RBBasicLintRuleTestData modifiesCollection.
	detector methodBlock: 
			[:context :result | 
			addSearcher executeTree: context parseTree initialAnswer: false.
			addSearcher answer 
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #private }
RBBasicDummyLintRuleTest class >> new [
	^super new
]

{ #category : #'unnecessary code' }
RBBasicDummyLintRuleTest class >> onlyReadOrWritten [
	| detector |
	detector := self new.
	detector name: 'Instance variables not read AND written'.
	detector result: nil references.
	detector classBlock: 
			[:context :result | 
			| allSubclasses |
			allSubclasses := context selectedClass withAllSubclasses.
			context selectedClass instVarNames do: 
					[:each | 
					| reads writes |
					reads := false.
					writes := false.
					allSubclasses detect: 
							[:class | 
							reads ifFalse: [reads := (class whichSelectorsRead: each) isEmpty not].
							writes ifFalse: [writes := (class whichSelectorsWrite: each) isEmpty not].
							reads & writes]
						ifNone: [result addInstVar: each for: context selectedClass]]].
	^detector
]

{ #category : #bugs }
RBBasicDummyLintRuleTest class >> overridesSpecialMessage [
	| detector |
	detector := self new.
	detector name: 'Overrides a "special" message'.
	detector resultClass: RBClassEnvironment.
	detector classBlock: 
			[:context :result | 
			((context selectedClass isMetaclass
				ifTrue: [self metaclassShouldNotOverride]
				ifFalse: [self classShouldNotOverride]) detect: 
						[:each | 
						context selectedClass superclass notNil and: 
								[(context selectedClass superclass canUnderstand: each)
									and: [context selectedClass includesSelector: each]]]
					ifNone: [nil]) notNil ifTrue: [result addClass: context selectedClass]].
	^detector
]

{ #category : #miscellaneous }
RBBasicDummyLintRuleTest class >> parseTreeSearcher [
	^ RBParseTreeSearcher new
]

{ #category : #miscellaneous }
RBBasicDummyLintRuleTest class >> precedence [
	| detector matcher |
	detector := self new.
	detector
		name: 'Inspect instances of "A + B * C" might be "A + (B * C)"'.
	matcher := self parseTreeSearcher.
	matcher
		addSearches:
			#('``@A + ``@B * ``@C' '``@A - ``@B * ``@C')
				-> [ :aNode :answer | answer or: [ aNode receiver parentheses isEmpty ] ].
	detector
		methodBlock: [ :context :result | 
			(matcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [ result addClass: context selectedClass selector: context selector ] ].
	^ detector
]

{ #category : #accessing }
RBBasicDummyLintRuleTest class >> protocolsToCheck [
	^#('bugs' 'possible bugs' 'unnecessary code' 'intention revealing' 'miscellaneous')
]

{ #category : #miscellaneous }
RBBasicDummyLintRuleTest class >> refersToClass [
	| detector |
	detector := self new.
	detector name: 'Refers to class name instead of "self class"'.
	detector classBlock: 
			[:context :result | 
			| sels className |
			className := context selectedClass theNonMetaClass name.
			sels := context selectedClass
						whichSelectorsReferTo: (Smalltalk associationAt: className).
			sels isEmpty ifFalse: 
					[result addSearchString: className.
					sels do: [:each | result addClass: context selectedClass selector: each]]].
	^detector
]

{ #category : #'possible bugs' }
RBBasicDummyLintRuleTest class >> returnsBooleanAndOther [
	| detector matcher |
	detector := self new.
	detector name: 'Returns a boolean and non boolean'.
	matcher := RBParseTreeSearcher new.
	matcher addSearch: '^``@xObject' -> 
					[:aNode :answer | 
					answer add: aNode value;
						yourself].
	detector methodBlock: 
			[:context :result | 
			| hasBool hasSelf |
			hasBool := false.
			hasSelf := context parseTree lastIsReturn not.
			(matcher executeTree: context parseTree initialAnswer: Set new) do: 
					[:each | 
					hasBool := hasBool or: 
									[(each isLiteralNode and: [{true. false} includes: each value])
										or: [each isMessage and: [#(#and: #or:) includes: each selector]]].
					hasSelf := hasSelf or: 
									[(each isVariable and: [each name = 'self'])
										or: [each isLiteralNode and: [({true. false} includes: each value) not]]]].
			hasSelf & hasBool
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #'possible bugs' }
RBBasicDummyLintRuleTest class >> returnsIfTrue [
	^self createParseTreeRule: 
				#('^`@condition ifTrue: [| `@temps | `@.statements]' 
				'^`@condition ifFalse: [| `@temps | `@.statements]') 
		name: 'Returns value of ifTrue:/ifFalse: without ifFalse:/ifTrue: block'
]

{ #category : #'possible bugs' }
RBBasicDummyLintRuleTest class >> sendsDifferentSuper [
	| detector |
	detector := self new.
	detector name: 'Sends different super message'.
	detector methodBlock: 
			[:context :result | 
			| message |
			(message := context superMessages detect: [:each | each ~= context selector]
						ifNone: [nil]) notNil ifTrue: 
					[result addSearchString: message.
					result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #bugs }
RBBasicDummyLintRuleTest class >> sentNotImplemented [

	| detector |

	detector := self new.
	detector name: 'Messages sent but not implemented'.
	detector
		methodBlock: [ :context :result | 
			| message |

			message := context messages detect: [ :each | ( context implements: each ) not ] ifNone: [ nil ].
			message
				ifNil: [ message := context superMessages
						detect: [ :each | 
							context selectedClass superclass isNil
								or: [ ( context selectedClass superclass canUnderstand: each ) not ]
							]
						ifNone: [ nil ].
					message
						ifNil: [ message := context selfMessages
								detect: [ :each | ( context selectedClass canUnderstand: each ) not ]
								ifNone: [ nil ]
							]
					].
			message notNil
				ifTrue: [ result addSearchString: message.
					result addClass: context selectedClass selector: context selector
					]
			].
	^ detector
]

{ #category : #'possible bugs' }
RBBasicDummyLintRuleTest class >> sentNotImplementedInApplication [

	| detector |

	detector := self new.
	detector name: 'Messages sent but not implemented in application'.
	detector
		methodBlock: [ :context :result | 
			| message class block |

			message := context messages
				detect: [ :each | ( context isItem: each in: context application ) not ]
				ifNone: [ nil ].
			class := context selectedClass.
			block := [ :each | 
			| app |

			app := context application.
			( self canCall: each in: class from: app ) not
			].
			message ifNil: [ message := context selfMessages detect: block ifNone: [ nil ] ].
			message
				ifNil: [ class := class superclass.
					class
						ifNil: [ context superMessages isEmpty
								ifFalse: [ message := context superMessages asArray first ]
							]
						ifNotNil: [ message := context superMessages detect: block ifNone: [ nil ] ]
					].
			message notNil
				ifTrue: [ result addSearchString: message.
					result addClass: context selectedClass selector: context selector
					]
			].
	^ detector
]

{ #category : #'intention revealing' }
RBBasicDummyLintRuleTest class >> sizeCheck [
	^self createParseTreeRule: (#(#do: #collect: #reject: #select:) collect: 
					[:each | 
					'`@object size > 0 ifTrue: [`@object ' , each
						, ' [:`each | | `@temps | `@.Statements1]. `@.Statements2]'])
				, (#(#do: #collect: #reject: #select:) collect: 
							[:each | 
							'`@object isEmpty ifFalse: [`@object ' , each
								, ' [:`each | | `@temps | `@.Statements1]. `@.Statements2]'])
		name: 'Unnecessary size check'
]

{ #category : #miscellaneous }
RBBasicDummyLintRuleTest class >> stringConcatenation [
	| detector matcher concatenationMatcher |
	detector := self new.
	detector name: 'String concatenation instead of streams'.
	matcher := RBParseTreeSearcher new.
	concatenationMatcher := RBParseTreeSearcher new.
	concatenationMatcher
		addSearch: '`@receiver , `@argument' -> [:aNode :answer | true].
	matcher
		addSearches: #('``@collection do: [:`each | | `@temps | ``@.Statements]' '``@collection do: [:`each | | `@temps | ``@.Statements] separatedBy: [| `@temps1 | ``@.Statements1]' '``@number to: ``@endNumber do: [:`i | | `@temps | ``@.Statements]' '``@collection detect: [:`each | | `@temps | ``@.Statements]' '``@collection detect: [:`each | | `@temps | ``@.Statements] ifNone: [| `@temps1 | ``@.Statements1]' '``@collection select: [:`each | | `@temps | ``@.Statements]' '``@collection inject: ``@value into: [:`each | | `@temps | ``@.Statements]')
				-> 
					[:aNode :answer | 
					answer or: 
							[(aNode arguments detect: 
									[:each | 
									each isBlock
										and: [concatenationMatcher executeTree: each initialAnswer: false]]
								ifNone: [nil]) notNil]].
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #private }
RBBasicDummyLintRuleTest class >> subclassOf: aClass overrides: aSelector [ 
	| subs |
	subs := aClass subclasses.
	1 to: subs size
		do: 
			[:i | 
			| each |
			each := subs at: i.
			(each includesSelector: aSelector) ifTrue: [^true].
			(self subclassOf: each overrides: aSelector) ifTrue: [^true]].
	^false
]

{ #category : #bugs }
RBBasicDummyLintRuleTest class >> subclassResponsibilityNotDefined [
	| detector subclassResponsibilitySymbol |
	detector := self new.
	subclassResponsibilitySymbol := 'subclassResponsibility' asSymbol.
	detector name: 'Subclass responsibility not defined'.
	detector classBlock: 
			[:context :result | 
			(context selectedClass whichSelectorsReferTo: subclassResponsibilitySymbol)
				do: 
					[:each | 
					(context selectedClass withAllSubclasses detect: 
							[:class | 
							class subclasses isEmpty
								and: [(class whichClassIncludesSelector: each) == context selectedClass]]
						ifNone: [nil]) notNil
						ifTrue: [result addClass: context selectedClass selector: each]]].
	^detector
]

{ #category : #private }
RBBasicDummyLintRuleTest class >> superMessages [
	^#(#release #postCopy #postBuildWith: #preBuildWith: #postOpenWith: #noticeOfWindowClose: #initialize)
]

{ #category : #'possible bugs' }
RBBasicDummyLintRuleTest class >> superSends [
	| detector |
	detector := self new.
	detector name: 'Missing super sends'.
	detector methodBlock: 
			[:context :result | 
			(context selectedClass isMetaclass not
				and: [self superMessages includes: context selector]) ifTrue: 
						[(context selectedClass superclass notNil
							and: [context selectedClass superclass canUnderstand: context selector])
								ifTrue: 
									[(context superMessages includes: context selector)
										ifFalse: [result addClass: context selectedClass selector: context selector]]]].
	^detector
]

{ #category : #'possible bugs' }
RBBasicDummyLintRuleTest class >> tempVarOverridesInstVar [
	| detector matcher vars varName |
	detector := self new.
	detector name: 'Instance variable overridden by temporary variable'.
	matcher := (RBParseTreeSearcher new) addArgumentSearch: '`xxxvar' -> 
								[:aNode :answer | 
								answer or: 
										[varName := aNode name.
										vars includes: varName]];
				yourself.
	detector methodBlock: 
			[:context :result | 
			vars := context instVarNames.
			(matcher executeTree: context parseTree initialAnswer: false) ifTrue: 
					[result addClass: context selectedClass selector: context selector.
					result addSearchString: varName]].
	^detector
]

{ #category : #'possible bugs' }
RBBasicDummyLintRuleTest class >> tempsReadBeforeWritten [
	| detector |
	detector := self new.
	detector name: 'Temporaries read before written'.
	detector methodBlock: 
			[:context :result | 
			| variables |
			variables := RBParseTreeSearcher nonBlockTempsIn: context parseTree.
			variables isEmpty 
				ifFalse: 
					[(RBReadBeforeWrittenTester variablesReadBeforeWrittenIn: context parseTree) 
						do: 
							[:each | 
							result addClass: context selectedClass selector: context selector.
							result addSearchString: each]]].
	^detector
]

{ #category : #'possible bugs' }
RBBasicDummyLintRuleTest class >> threeElementPoint [
	| detector matcher |
	detector := self new.
	detector name: 'Possible three element point (e.g., x @ y + q @ r)'.
	matcher := (RBParseTreeSearcher new) addSearch: '``@x @ ``@y' -> 
								[:aNode :answer | 
								answer or: 
										[| current |
										current := aNode parent.
										[current isNil or: 
												[current isMessage
													and: [current selector == #@ or: [current selector isInfix not]]]]
											whileFalse: [current := current parent].
										(current isNil or: [current isMessage and: [current selector isInfix not]])
											not]];
				yourself.
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #'intention revealing' }
RBBasicDummyLintRuleTest class >> toDo [
	| detector matcher |
	detector := self new.
	detector name: 'Uses to:do: instead of do:, with:do: or timesRepeat:'.
	matcher := RBParseTreeSearcher new.
	matcher
		addSearch: '1 to: ``@object size do: [:`each | | `@temps | `@.Statements]' -> 
					[:aNode :answer | 
					answer or: 
							[| varName variableMatcher |
							varName := aNode arguments last arguments first name. "`each"
							variableMatcher := RBParseTreeSearcher new.
							variableMatcher addSearch: varName
										-> [:node :ans | ans and: [node parent isMessage and: [node parent selector == #at:]]].
							variableMatcher executeTree: aNode arguments last body initialAnswer: true]].
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #'unnecessary code' }
RBBasicDummyLintRuleTest class >> unreferencedVariables [
	| detector |
	detector := self new.
	detector name: 'Variables not referenced'.
	detector result: nil unreferenced.
	detector classBlock: 
			[:context :result | 
			| allSubclasses |
			allSubclasses := context selectedClass withAllSubclasses.
			context selectedClass instVarNames do: 
					[:each | 
					allSubclasses
						detect: [:class | (class whichSelectorsAccess: each) isEmpty not]
						ifNone: [result addInstVar: each for: context selectedClass]].
			context selectedClass isMetaclass ifFalse: 
					[context selectedClass classPool associationsDo: 
							[:each | 
							(SystemNavigation default allCallsOn: each) ifEmpty:  [result addClassVar: each key for: context selectedClass]]]].
	^detector
]

{ #category : #'possible bugs' }
RBBasicDummyLintRuleTest class >> usesAdd [
	| detector addSearcher |
	detector := self new.
	detector name: 'Uses the result of an add: message'.
	addSearcher := RBParseTreeSearcher usesResultOfAdd.
	detector methodBlock: 
			[:context :result | 
			(addSearcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #bugs }
RBBasicDummyLintRuleTest class >> usesTrue [
	| detector trueBinding falseBinding |
	detector := self new.
	trueBinding := Smalltalk associationAt: #True.
	falseBinding := Smalltalk associationAt: #False.
	detector name: 'Uses True/False instead of true/false'.
	detector methodBlock: 
			[:context :result | 
			| method |
			method := context compiledMethod.
			((method referencesLiteral: trueBinding)
				or: [method referencesLiteral: falseBinding]) ifTrue: 
						[result addClass: context selectedClass selector: context selector.
						result searchStrings: #('True' 'False')]].
	^detector
]

{ #category : #miscellaneous }
RBBasicDummyLintRuleTest class >> utilityMethods [
	| detector |
	detector := self new.
	detector name: 'Utility methods'.
	detector methodBlock: 
			[:context :result | 
			(context selectedClass isMetaclass | (context selector numArgs == 0) or: 
					[(context protocols detect: 
							[:each | 
							(self utilityProtocols detect: [:protocol | protocol match: each]
								ifNone: [nil]) notNil]
						ifNone: [nil]) notNil])
				ifFalse: 
					[(self subclassOf: context selectedClass overrides: context selector)
						ifFalse: 
							[(context superMessages isEmpty and: [context selfMessages isEmpty]) ifTrue: 
									[(RBParseTreeSearcher
										references: context selectedClass allInstVarNames
												, context selectedClass allClassVarNames asArray , #('self')
										in: context parseTree) isEmpty
										ifTrue: [result addClass: context selectedClass selector: context selector]]]]].
	^detector
]

{ #category : #private }
RBBasicDummyLintRuleTest class >> utilityProtocols [
	"If a method is defined in one of these protocols, then don't check if its a utility method."

	^#('*utilit*')
]

{ #category : #miscellaneous }
RBBasicDummyLintRuleTest class >> variableAssignedLiteral [
	| detector |
	detector := self new.
	detector name: 'Variable is only assigned a single literal value'.
	detector result: nil references.
	detector classBlock: 
			[:context :result | 
			| allSubclasses |
			allSubclasses := context selectedClass withAllSubclasses.
			context selectedClass instVarNames do: 
					[:each | 
					| defClass selector |
					(allSubclasses inject: 0
						into: 
							[:sum :class | 
							| sels |
							sels := class whichSelectorsWrite: each.
							sels size == 1 ifTrue: 
									[selector := sels asArray first.
									defClass := class].
							sum + sels size])
							== 1 ifTrue: 
								[| tree searcher |
								searcher := RBParseTreeSearcher new.
								searcher addSearch: (each , ' := ``@object')
											-> [:aNode :answer | answer isNil and: [aNode value isLiteralNode]].
								tree := defClass parseTreeFor: selector.
								tree notNil ifTrue: 
										[(searcher executeTree: tree initialAnswer: nil) == true
											ifTrue: [result addInstVar: each for: context selectedClass]]]]].
	^detector
]

{ #category : #'unnecessary code' }
RBBasicDummyLintRuleTest class >> variableReferencedOnce [
	| detector |
	detector := self new.
	detector 
		name: 'Variable referenced in only one method and always assigned first'.
	detector classBlock: 
			[:context :result | 
			| allSubclasses |
			allSubclasses := context selectedClass withAllSubclasses.
			context selectedClass instVarNames do: 
					[:each | 
					| defClass selector |
					(allSubclasses inject: 0
						into: 
							[:sum :class | 
							| sels |
							sels := class whichSelectorsAccess: each.
							sels size == 1 
								ifTrue: 
									[selector := sels asArray first.
									defClass := class].
							sum + sels size]) 
							== 1 
							ifTrue: 
								[| tree |
								tree := defClass parseTreeFor: selector.
								tree notNil 
									ifTrue: 
										[(RBReadBeforeWrittenTester readBeforeWritten: (Array with: each) in: tree) 
											isEmpty 
												ifTrue: 
													[result addClass: defClass selector: selector.
													result addSearchString: each]]]]].
	^detector
]

{ #category : #'intention revealing' }
RBBasicDummyLintRuleTest class >> whileTrue [
	^self createParseTreeRule: 
			#('| `@temps | 
				`@.Statements1. 
				[`index <= `@stop] 
					whileTrue: 
						[| `@blockTemps | 
						`@.BlockStmts1. 
						`index := `index + 1].
				`@.Statements2' 
			'| `@temps | 
				`@.Statements1. 
				[`index < `@stop] 
					whileTrue: 
						[| `@blockTemps | 
						`@.BlockStmts1. 
						`index := `index + 1].
				`@.Statements2'
			'| `@temps | 
				`@.Statements1. 
				[`index >= `@stop] 
					whileTrue: 
						[| `@blockTemps | 
						`@.BlockStmts1. 
						`index := `index - 1].
				`@.Statements2' 
			'| `@temps | 
				`@.Statements1. 
				[`index > `@stop] 
					whileTrue: 
						[| `@blockTemps | 
						`@.BlockStmts1. 
						`index := `index - 1].
				`@.Statements2') 
		name: 'Uses whileTrue: instead of to:do:'
]

{ #category : #miscellaneous }
RBBasicDummyLintRuleTest class >> yourselfNotUsed [
	| detector addSearcher |
	detector := self new.
	detector name: 'Doesn''t use the result of a yourself message'.
	addSearcher := RBParseTreeSearcher new.
	addSearcher addSearch: '`@object yourself'
				-> [:aNode :answer | answer or: [aNode isUsed not]].
	detector methodBlock: 
			[:context :result | 
			(addSearcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #initialization }
RBBasicDummyLintRuleTest >> anInstVar [
	^ anInstVar
]

{ #category : #'initialize-release' }
RBBasicDummyLintRuleTest >> anInstVar: anObject [
	anInstVar := anObject
]

{ #category : #accessing }
RBBasicDummyLintRuleTest >> checkClass: aSmalllintContext [ 
	^classBlock value: aSmalllintContext value: result
]

{ #category : #accessing }
RBBasicDummyLintRuleTest >> checkMethod: aSmalllintContext [ 
	^methodBlock value: aSmalllintContext value: result
]

{ #category : #initialization }
RBBasicDummyLintRuleTest >> classBlock [
	^ self anInstVar + 5
]

{ #category : #initialization }
RBBasicDummyLintRuleTest >> classBlock: aBlock [
	classBlock := aBlock testMethod1
]

{ #category : #accessing }
RBBasicDummyLintRuleTest >> foobar [

	^#( true false )
]

{ #category : #initialization }
RBBasicDummyLintRuleTest >> initialize [
	super initialize.
	self anInstVar: 1.
	classBlock := [:context :aResult | ].
	methodBlock := [:context :aResult | ].
	self resultClass: RBSelectorEnvironment
]

{ #category : #testing }
RBBasicDummyLintRuleTest >> isComposite [
	^false
]

{ #category : #testing }
RBBasicDummyLintRuleTest >> isEmpty [
	^result isEmpty
]

{ #category : #private }
RBBasicDummyLintRuleTest >> method: anObject1 with: anObject2 lots: anObject3 of: anObject4 arguments: anObject5 [
	^ anObject5 + anObject1 > (anObject4 - anObject2 + anObject3)
]

{ #category : #initialization }
RBBasicDummyLintRuleTest >> methodBlock: aBlock [
	methodBlock := aBlock
]

{ #category : #initialization }
RBBasicDummyLintRuleTest >> newResultClass: aClass [ 
"New one:)"
	result := aClass new
]

{ #category : #accessing }
RBBasicDummyLintRuleTest >> problemCount [
	^result problemCount
]

{ #category : #'initialize-release' }
RBBasicDummyLintRuleTest >> resetResult [
	result := result copyEmpty.
	result label: name
]

{ #category : #accessing }
RBBasicDummyLintRuleTest >> result [
	^result
]

{ #category : #'initialize-release' }
RBBasicDummyLintRuleTest >> result: aResult [ 
	result := aResult copyEmpty
]

{ #category : #initialization }
RBBasicDummyLintRuleTest >> resultClass: aClass [ 
	result := aClass new
]

{ #category : #accessing }
RBBasicDummyLintRuleTest >> someDemoMethod [
	^ self junk
]

{ #category : #private }
RBBasicDummyLintRuleTest >> viewResults [
	result openEditor
]
