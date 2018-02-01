"
SUnit tests for method pragmas
"
Class {
	#name : #MethodPragmaTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Misc'
}

{ #category : #utilities }
MethodPragmaTest >> assertPragma: aString givesKeyword: aSymbol arguments: anArray [
	| pragma  |
	pragma := self pragma: aString selector: #zork.
	self assert: pragma keyword = aSymbol.
	self assert: pragma arguments = anArray.

]

{ #category : #utilities }
MethodPragmaTest >> compile: aString selector: aSelector [
	self class 
		compileSilently: aSelector , String lf , aString
		classified: self methodCategory.
	^ self class >> aSelector.
]

{ #category : #utilities }
MethodPragmaTest >> methodCategory [
	^ #generated
]

{ #category : #utilities }
MethodPragmaTest >> pragma: aString selector: aSelector [
	^ (self compile: '<' , aString , '>' selector: aSelector)
		pragmas first.
]

{ #category : #utilities }
MethodPragmaTest >> pragma: aSymbol selector: aSelector times: anInteger [
	^ (self 
		compile: (String streamContents: [ :stream | 
			(1 to: anInteger) asArray shuffled do: [ :each | 
				stream 
					nextPut: $<; nextPutAll: aSymbol; space;
					print: each; nextPut: $>; cr ] ])
		selector: aSelector)
			pragmas.
]

{ #category : #running }
MethodPragmaTest >> tearDown [
	(self class organization listAtCategoryNamed: self methodCategory)
		do: [ :each | self class removeSelectorSilently: each ].
	self class organization removeCategory: self methodCategory.
	super tearDown
]

{ #category : #'testing-finding' }
MethodPragmaTest >> testAllNamedFromTo [
	| pragmasCompiled pragmasDetected |
	pragmasCompiled := self pragma: #foo: selector: #bar times: 5.
	pragmasDetected := Pragma allNamed: #foo: from: self class to: Object.
	self assert: pragmasDetected = pragmasCompiled.
	
	pragmasDetected := Pragma allNamed: #foo: from: Object to: Object.
	self assert: pragmasDetected isEmpty.
]

{ #category : #'testing-finding' }
MethodPragmaTest >> testAllNamedFromToSortedByArgument [
	| pragmasCompiled pragmasDetected |
	pragmasCompiled := self pragma: #foo: selector: #bar times: 5.
	pragmasDetected := Pragma allNamed: #foo: from: self class to: Object sortedByArgument: 1.
	self assert: pragmasDetected = (pragmasCompiled 
		sort: [ :a :b | (a argumentAt: 1) < (b argumentAt: 1) ])
]

{ #category : #'testing-finding' }
MethodPragmaTest >> testAllNamedFromToSortedUsing [
	| pragmasCompiled pragmasDetected |
	pragmasCompiled := self pragma: #foo: selector: #bar times: 5.
	pragmasDetected := Pragma 
		allNamed: #foo: from: self class to: Object 
		sortedUsing: [ :a :b | (a argumentAt: 1) > (b argumentAt: 1) ].
	self assert: pragmasDetected = (pragmasCompiled 
		sort: [ :a :b | (a argumentAt: 1) > (b argumentAt: 1) ]).
]

{ #category : #'testing-finding' }
MethodPragmaTest >> testAllNamedIn [
	| pragmasCompiled pragmasDetected |
	pragmasCompiled := self pragma: #foo: selector: #bar times: 5.
	pragmasDetected := Pragma allNamed: #foo: in: self class.
	self assert: pragmasDetected = pragmasCompiled.
	
	pragmasDetected := Pragma allNamed: #foo: in: Object.
	self assert: pragmasDetected isEmpty.
]

{ #category : #'testing-finding' }
MethodPragmaTest >> testAllNamedInSortedByArgument [
	| pragmasCompiled pragmasDetected |
	pragmasCompiled := self pragma: #foo: selector: #bar times: 5.
	pragmasDetected := Pragma allNamed: #foo: in: self class sortedByArgument: 1.
	self assert: pragmasDetected = (pragmasCompiled 
		sort: [ :a :b | (a argumentAt: 1) < (b argumentAt: 1) ])
]

{ #category : #'testing-finding' }
MethodPragmaTest >> testAllNamedInSortedUsing [
	| pragmasCompiled pragmasDetected |
	pragmasCompiled := self pragma: #foo: selector: #bar times: 5.
	pragmasDetected := Pragma 
		allNamed: #foo: in: self class 
		sortedUsing: [ :a :b | (a argumentAt: 1) > (b argumentAt: 1) ].
	self assert: pragmasDetected = (pragmasCompiled 
		sort: [ :a :b | (a argumentAt: 1) > (b argumentAt: 1) ]).
]

{ #category : #testing }
MethodPragmaTest >> testArgumentAt [
	| pragma |
	pragma := Pragma keyword: #value:value:value: arguments: #( 3 2 1 ).
	self assert: (pragma argumentAt: 1) = 3.
	self assert: (pragma argumentAt: 2) = 2.
	self assert: (pragma argumentAt: 3) = 1
]

{ #category : #'testing-pragma' }
MethodPragmaTest >> testArguments [
	| pragma |
	pragma := Pragma keyword: #foo: arguments: #( 123 ).
	self assert: pragma arguments = #( 123 ).
]

{ #category : #'testing-compiler' }
MethodPragmaTest >> testCompileArray [
	self assertPragma: 'foo: #()' givesKeyword: #foo: arguments: #( () ).
	self assertPragma: 'foo: #( foo )' givesKeyword: #foo: arguments: #( ( foo ) ).
	self assertPragma: 'foo: #( foo: )' givesKeyword: #foo: arguments: #( ( foo: ) ).
	self assertPragma: 'foo: #( 12 )' givesKeyword: #foo: arguments: #( ( 12 ) ).
	self assertPragma: 'foo: #( true )' givesKeyword: #foo: arguments: #( ( true ) ).
	
]

{ #category : #'testing-compiler' }
MethodPragmaTest >> testCompileBinary [
	self assertPragma: ' = 1' givesKeyword: #= arguments: #( 1 ).
	self assertPragma: ' , 3' givesKeyword: #, arguments: #( 3 ).
	self assertPragma: ' > 4' givesKeyword: #> arguments: #( 4 ).
	self assertPragma: ' < 5' givesKeyword: #< arguments: #( 5 ).

	self assertPragma: ' == 1' givesKeyword: #== arguments: #( 1 ).
	self assertPragma: ' <> 3' givesKeyword: #<> arguments: #( 3 ).
	self assertPragma: ' >< 4' givesKeyword: #>< arguments: #( 4 ).
	self assertPragma: ' ** 5' givesKeyword: #** arguments: #( 5 )
]

{ #category : #'testing-compiler' }
MethodPragmaTest >> testCompileCharacter [
	self assertPragma: 'foo: $a' givesKeyword: #foo: arguments: #( $a ).
	self assertPragma: 'foo: $ ' givesKeyword: #foo: arguments: #( $  ).
]

{ #category : #'testing-compiler' }
MethodPragmaTest >> testCompileEmpty [
	self assertPragma: 'foo' givesKeyword: #foo arguments: #().
]

{ #category : #'testing-compiler' }
MethodPragmaTest >> testCompileFull [
	self assertPragma: 'foo: 1' givesKeyword: #foo: arguments: #( 1 ).
	self assertPragma: 'foo: 1 bar: 2' givesKeyword: #foo:bar: arguments: #( 1 2 ).
]

{ #category : #'testing-compiler' }
MethodPragmaTest >> testCompileInvalid [
	"Invalid pragmas should properly raise an error."

	self should: [ self compile: '<>' selector: #zork ] raise: SyntaxErrorNotification.
	self should: [ self compile: '<1>' selector: #zork ] raise: SyntaxErrorNotification.	
	self should: [ self compile: '<#123>' selector: #zork ] raise: SyntaxErrorNotification.
	
	self should: [ self compile: '<foo bar>' selector: #zork ] raise: SyntaxErrorNotification.
	self should: [ self compile: '<foo 1>' selector: #zork ] raise: SyntaxErrorNotification.
	self should: [ self compile: '<foo bar zork>' selector: #zork ] raise: SyntaxErrorNotification.
	self should: [ self compile: '<foo bar 1>' selector: #zork ] raise: SyntaxErrorNotification.
	
	self should: [ self compile: '<foo: #bar: zork:>' selector: #zork ] raise: SyntaxErrorNotification.
	
	self should: [ self compile: '<<1>' selector: #zork ] raise: SyntaxErrorNotification.
	self should: [ self compile: '<=2>' selector: #zork ] raise: SyntaxErrorNotification.

	self should: [ self compile: '< =1 = >' selector: #zork ] raise: SyntaxErrorNotification.
	self should: [ self compile: '< =1 =2 >' selector: #zork ] raise: SyntaxErrorNotification.
]

{ #category : #'testing-compiler' }
MethodPragmaTest >> testCompileNumber [
	self assertPragma: 'foo: 123' givesKeyword: #foo: arguments: #( 123 ).
	self assertPragma: 'foo: -123' givesKeyword: #foo: arguments: #( -123 ).
	self assertPragma: 'foo: 12.3' givesKeyword: #foo: arguments: #( 12.3 ).
	self assertPragma: 'foo: -12.3' givesKeyword: #foo: arguments: #( -12.3 ).
]

{ #category : #'testing-compiler' }
MethodPragmaTest >> testCompileString [
	self assertPragma: 'foo: ''''' givesKeyword: #foo: arguments: #( '' ).
	self assertPragma: 'foo: ''bar''' givesKeyword: #foo: arguments: #( 'bar' ).
]

{ #category : #'testing-compiler' }
MethodPragmaTest >> testCompileSymbol [
	self assertPragma: 'foo: #bar' givesKeyword: #foo: arguments: #( bar ).
	self assertPragma: 'foo: #bar:' givesKeyword: #foo: arguments: #( bar: ).
	self assertPragma: 'foo: #bar:zork:' givesKeyword: #foo: arguments: #( bar:zork: ).
]

{ #category : #'testing-compiler' }
MethodPragmaTest >> testCompileTemps [
	"Pragmas should be placeable before and after temps."

	self assert: (self compile: '| temps | <foo>' selector: #zork) pragmas notEmpty.
	self assert: (self compile: '<foo> | temps |' selector: #zork) pragmas notEmpty
]

{ #category : #'testing-compiler' }
MethodPragmaTest >> testCompileValue [
	self assertPragma: 'foo: true' givesKeyword: #foo: arguments: #( true ).
	self assertPragma: 'foo: false' givesKeyword: #foo: arguments: #( false ).
	self assertPragma: 'foo: nil' givesKeyword: #foo: arguments: #( nil )
]

{ #category : #'testing-pragma' }
MethodPragmaTest >> testKeyword [
	| pragma |
	pragma := Pragma keyword: #foo: arguments: #( 123 ).
	self assert: pragma keyword = #foo:.
]

{ #category : #'testing-pragma' }
MethodPragmaTest >> testMessage [
	| pragma message |
	pragma := Pragma keyword: #foo: arguments: #( 123 ).
	message := pragma message.
	
	self assert: message selector = #foo:.
	self assert: message arguments = #( 123 ).
]

{ #category : #'testing-method' }
MethodPragmaTest >> testMethod [
	| pragma |
	pragma := self pragma: 'foo' selector: #bar.
	self assert: pragma method == (self class >> #bar).
]

{ #category : #'testing-method' }
MethodPragmaTest >> testMethodClass [
	| pragma |
	pragma := self pragma: 'foo' selector: #bar.
	self assert: pragma methodClass == self class.
]

{ #category : #'testing-method' }
MethodPragmaTest >> testMethodSelector [
	| pragma |
	pragma := self pragma: 'foo' selector: #bar.
	self assert: pragma methodSelector == #bar.
]

{ #category : #'testing-compiled' }
MethodPragmaTest >> testNoPragma [
	| method |
	method := self compile: '' selector: #foo.
	self assert: method pragmas = #().
]

{ #category : #testing }
MethodPragmaTest >> testNumArgs [
	| pragma |
	pragma := Pragma keyword: #value arguments: #().
	self assert: pragma numArgs = 0.
	
	pragma := Pragma keyword: #+ arguments: #( 1 ).
	self assert: pragma numArgs = 1.
	
	pragma := Pragma keyword: #value:value: arguments: #( 1 2 ).
	self assert: pragma numArgs = 2
]

{ #category : #'testing-primitives' }
MethodPragmaTest >> testPrimitiveIndexed1 [
	"This test useses the #instVarAt: primitive."
	
	self compile: '<primitive: 74> ^ #inst' selector: #inst.
	self assert: self inst = #inst.
]

{ #category : #'testing-primitives' }
MethodPragmaTest >> testPrimitiveIndexed2 [
	"This test useses the #identityHash primitive."

	self compile: '<primitive: 75> ^ #idHash' selector: #idHash.
	self assert: self idHash = self basicIdentityHash.
]

{ #category : #'testing-primitives' }
MethodPragmaTest >> testPrimitiveNamed1 [
	"This test useses the #primitiveDirectoryLookup primitive."

	self compile: '<primitive: ''primitiveDirectoryLookup'' module: ''FilePlugin''> ^ #lookup' selector: #lookup.
	self assert: self lookup equals: #lookup.
	

]

{ #category : #'testing-primitives' }
MethodPragmaTest >> testPrimitiveNamed2 [
	"This test useses the #primPathNameDelimiter primitive."

	self compile: '<primitive: ''primitiveDirectoryDelimitor'' module: ''FilePlugin''> ^ #delim' selector: #delim.
	self assert: self delim equals: FilePluginPrims new delimiter.
	

]

{ #category : #'testing-primitives' }
MethodPragmaTest >> testPrimitiveNamedErrorCode2 [
	"This test useses the #primitiveDirectoryLookup primitive."

	self compile: '<primitive: ''primitiveDirectoryLookup'' module: ''FilePlugin'' error: errorCode> ^ errorCode' selector: #lookup.
	self assert: self lookup equals: nil.
	

]

{ #category : #testing }
MethodPragmaTest >> testSendTo [
	| pragma wasHere |
	pragma := Pragma keyword: #value:value: arguments: #( 1 2 ).
	self assert: (pragma sendTo: [ :a :b |
		self assert: a = 1; assert: b = 2.
		wasHere := true ]).
	self assert: wasHere
]

{ #category : #testing }
MethodPragmaTest >> testWithArgumentsDo [
	| pragma wasHere |
	pragma := Pragma keyword: #add:after: arguments: #( 1 2 ).
	self assert: (pragma withArgumentsDo: [ :a :b |
		self assert: a = 1; assert: b = 2.
		wasHere := true ]).
	self assert: wasHere
]
