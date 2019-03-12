Class {
	#name : #RBBasicLintRuleTestData,
	#superclass : #RBLintRuleTestData,
	#instVars : [
		'methodBlock',
		'result',
		'classBlock',
		'anInstVar'
	],
	#category : #'Refactoring-Tests-Core-Data'
}

{ #category : #private }
RBBasicLintRuleTestData class >> canCall: aSelector in: aClass from: anApplication [
	"This method contains on purpose not implemented messages, such as rootApplication "
	| methodApp root |
	(aClass canUnderstand: aSelector) ifFalse: [^false].
	root := anApplication rootApplication.
	methodApp := ((aClass whichClassIncludesSelector: aSelector) compiledMethodAt: aSelector)
				application rootApplication.
	^methodApp == root or: [root isBasedOn: methodApp]
]

{ #category : #private }
RBBasicLintRuleTestData class >> classShouldNotOverride [
	^#(#== #class)
]

{ #category : #private }
RBBasicLintRuleTestData class >> createMatcherFor: codeStrings method: aBoolean [ 
	| matcher |
	matcher := RBParseTreeSearcher new.
	aBoolean
		ifTrue: [matcher addMethodSearches: codeStrings -> [:aNode :answer | true]]
		ifFalse: [matcher addSearches: codeStrings -> [:aNode :answer | true]].
	^matcher
]

{ #category : #'instance creation' }
RBBasicLintRuleTestData class >> createParseTreeRule: codeStrings method: aBoolean name: aName [ 
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
RBBasicLintRuleTestData class >> createParseTreeRule: codeStrings name: aName [ 
	^self createParseTreeRule: codeStrings
		method: false
		name: aName
]

{ #category : #'unnecessary code' }
RBBasicLintRuleTestData class >> justSendsSuper [
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
RBBasicLintRuleTestData class >> longMethodSize [
	^10
]

{ #category : #private }
RBBasicLintRuleTestData class >> metaclassShouldNotOverride [
	^#(#name #comment)
]

{ #category : #'possible bugs' }
RBBasicLintRuleTestData class >> modifiesCollection [
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
RBBasicLintRuleTestData class >> new [
	^super new
]

{ #category : #miscellaneous }
RBBasicLintRuleTestData class >> precedence [
	| detector matcher |
	detector := self new.
	detector name: 'Inspect instances of "A + B * C" might be "A + (B * C)"'.
	matcher := RBParseTreeSearcher new.
	matcher addSearches: #('``@A + ``@B * ``@C' '``@A - ``@B * ``@C')
				-> [:aNode :answer | answer or: [aNode receiver parentheses isEmpty]].
	detector methodBlock: 
			[:context :result | 
			(matcher executeTree: context parseTree initialAnswer: false)
				ifTrue: [result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #accessing }
RBBasicLintRuleTestData class >> protocolsToCheck [
	^#('bugs' 'possible bugs' 'unnecessary code' 'intention revealing' 'miscellaneous')
]

{ #category : #'possible bugs' }
RBBasicLintRuleTestData class >> sentNotImplementedInApplication [
	| detector |
	detector := self new.
	detector name: 'Messages sent but not implemented in application'.
	detector methodBlock: 
			[:context :result | 
			| message class block |
			message := context messages
						detect: [:each | (context isItem: each in: context application) not]
						ifNone: [nil].
			class := context selectedClass.
			block := 
					[:each | 
					| app |
					app := context application.
					(self canCall: each
						in: class
						from: app) not].
			message isNil ifTrue: [message := context selfMessages detect: block ifNone: [nil]].
			message isNil ifTrue: 
					[class := class superclass.
					class isNil
						ifTrue: 
							[context superMessages isEmpty ifFalse: [message := context superMessages asArray first]]
						ifFalse: [message := context superMessages detect: block ifNone: [nil]]].
			message notNil ifTrue: 
					[result addSearchString: message.
					result addClass: context selectedClass selector: context selector]].
	^detector
]

{ #category : #private }
RBBasicLintRuleTestData class >> subclassOf: aClass overrides: aSelector [ 
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
RBBasicLintRuleTestData class >> subclassResponsibilityNotDefined [
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
RBBasicLintRuleTestData class >> superMessages [
	^#(#release #postCopy #postBuildWith: #preBuildWith: #postOpenWith: #noticeOfWindowClose: #initialize)
]

{ #category : #'possible bugs' }
RBBasicLintRuleTestData class >> superSends [
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

{ #category : #'intention revealing' }
RBBasicLintRuleTestData class >> toDo [
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

{ #category : #miscellaneous }
RBBasicLintRuleTestData class >> utilityMethods [
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
RBBasicLintRuleTestData class >> utilityProtocols [
	"If a method is defined in one of these protocols, then don't check if its a utility method."

	^#('*utilit*')
]

{ #category : #initialization }
RBBasicLintRuleTestData >> anInstVar [
	^ anInstVar
]

{ #category : #'initialize-release' }
RBBasicLintRuleTestData >> anInstVar: anObject [
	anInstVar := anObject
]

{ #category : #accessing }
RBBasicLintRuleTestData >> checkClass: aSmalllintContext [ 
	^classBlock value: aSmalllintContext value: result
]

{ #category : #accessing }
RBBasicLintRuleTestData >> checkMethod: aSmalllintContext [ 
	^methodBlock value: aSmalllintContext value: result
]

{ #category : #initialization }
RBBasicLintRuleTestData >> classBlock [
	^ self anInstVar + 5
]

{ #category : #initialization }
RBBasicLintRuleTestData >> classBlock: aBlock [
	classBlock := aBlock testMethod1
]

{ #category : #accessing }
RBBasicLintRuleTestData >> foobar [

	^#( true false )
]

{ #category : #initialization }
RBBasicLintRuleTestData >> initialize [
	super initialize.
	self anInstVar: 1.
	classBlock := [:context :aResult | ].
	methodBlock := [:context :aResult | ].
	self resultClass: RBSelectorEnvironment
]

{ #category : #testing }
RBBasicLintRuleTestData >> isEmpty [
	^result isEmpty
]

{ #category : #private }
RBBasicLintRuleTestData >> method: anObject1 with: anObject2 lots: anObject3 of: anObject4 arguments: anObject5 [
	^ anObject5 + anObject1 > (anObject4 - anObject2 + anObject3)
]

{ #category : #initialization }
RBBasicLintRuleTestData >> methodBlock: aBlock [
	methodBlock := aBlock
]

{ #category : #initialization }
RBBasicLintRuleTestData >> newResultClass: aClass [ 
"New one:)"
	result := aClass new
]

{ #category : #accessing }
RBBasicLintRuleTestData >> problemCount [
	^result problemCount
]

{ #category : #'initialize-release' }
RBBasicLintRuleTestData >> resetResult [
	result := result copyEmpty.
	result label: name
]

{ #category : #accessing }
RBBasicLintRuleTestData >> result [
	^result
]

{ #category : #'initialize-release' }
RBBasicLintRuleTestData >> result: aResult [ 
	result := aResult copyEmpty
]

{ #category : #initialization }
RBBasicLintRuleTestData >> resultClass: aClass [ 
	result := aClass new
]

{ #category : #accessing }
RBBasicLintRuleTestData >> someDemoMethod [
	^ self junk
]

{ #category : #private }
RBBasicLintRuleTestData >> viewResults [
	result openEditor
]
