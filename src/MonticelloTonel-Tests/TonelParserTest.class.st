Class {
	#name : #TonelParserTest,
	#superclass : #MCTestCase,
	#category : #'MonticelloTonel-Tests'
}

{ #category : #asserting }
TonelParserTest >> assertParse: aString rule: rule equals: result [ 
	self 
		assert: (self parse: aString rule: rule)
		equals: result
]

{ #category : #private }
TonelParserTest >> parse: aString rule: rule [
	| parser |
	
	parser := TonelParser on: aString readStream.
	 ^ parser perform: rule.
	
]

{ #category : #asserting }
TonelParserTest >> shouldParse: aString rule: rule raise: error [ 
	| parser |
	
	parser := TonelParser on: aString readStream.
	self 
		should: [ parser perform: rule ]
		raise: error
]

{ #category : #tests }
TonelParserTest >> testComment [
	self 
		shouldParse: 'this "should" be an error'
		rule: #comment
		raise: TonelParseError.
	
	self  
		assertParse: '"this is a comment"'
		rule: #comment
		equals: 'this is a comment'.
	
	self  
		assertParse: '"""this"" is a comment with ""nested"" colons ""also at the end"""'
		rule: #comment
		equals: '"this" is a comment with "nested" colons "also at the end"'	
		
		

]

{ #category : #tests }
TonelParserTest >> testExtractSelector [
	| parser |
	
	parser := TonelParser new.
	
	self assert: (parser extractSelector: 'unary') equals: #unary.
	self assert: (parser extractSelector: '+ something') equals: #+.
	self assert: (parser extractSelector: '==> other') equals: #==>.
	self 
		assert: (parser extractSelector: 'some: arg1 keyword: arg2 selector: arg3') 
		equals: #some:keyword:selector:.
	self 
		assert: (parser extractSelector: 'some: 	arg1 keyword:arg2 selector: arg3') 
		equals: #some:keyword:selector:.
	self 
		assert: (parser extractSelector: 'some: arg1 
keyword: arg2 
selector: arg3') 
		equals: #some:keyword:selector:.
]

{ #category : #tests }
TonelParserTest >> testExtractSelectorWithComments [
	| parser |
	
	parser := TonelParser new.
	
	self assert: (parser extractSelector: 'unary "comment"') equals: #unary.
	self assert: (parser extractSelector: '+ "comment" something') equals: #+.
	self assert: (parser extractSelector: '==> "comment" other') equals: #==>.
	self 
		assert: (parser extractSelector: 'some: arg1 "comment"  keyword: arg2  "comment" selector: arg3 "comment"') 
		equals: #some:keyword:selector:.
	self 
		assert: (parser extractSelector: 'some: 	arg1 "comment" keyword:arg2 selector: arg3') 
		equals: #some:keyword:selector:.
	self 
		assert: (parser extractSelector: 'some: arg1 "comment"
keyword: arg2 "comment"
selector: arg3') 
		equals: #some:keyword:selector:.
]

{ #category : #tests }
TonelParserTest >> testMetadata [
	self 
		assertParse: '{ #vars: [ #a, #b ] }' 
		rule: #metadata 
		equals: { #vars -> #(a b) } asDictionary.
	
	self 
		assertParse: '{ 
	#vars: [ #a, #b ],
	#uses: { #someNested: 42 } 
	}' 
		rule: #metadata 
		equals: { 
			#vars -> #(a b). 
			#uses -> { #someNested -> 42 } asDictionary
		} asDictionary

]

{ #category : #tests }
TonelParserTest >> testMethod [
	self 
		assertParse: 'Object>>name' 
		rule: #method
		equals: #(('Object' nil) 'name').
		
	self 
		assertParse: 'Object >> name: aString' 
		rule: #method
		equals: #(('Object' nil) 'name: aString').
		
	self 
		assertParse: 'Object >> name: aString [ I do not care ]' 
		rule: #method
		equals: #(('Object' nil) 'name: aString').
		
	self 
		assertParse: 'Object class >> name: aString' 
		rule: #method
		equals: #(('Object' 'class') 'name: aString').
]

{ #category : #tests }
TonelParserTest >> testMethodBody [
	self 
		assertParse: '[ method body... I will ignore what is inside ]'
		rule: #methodBody
		equals: ' method body... I will ignore what is inside'.

	self 
		assertParse: '[
method 
	[body... [I 
		will ignore] 
what] is inside
]'
		rule: #methodBody
		equals: '
method 
	[body... [I 
		will ignore] 
what] is inside'.
		
	self 
		assertParse: '[ method body with "''", ''"'', "[", '']'' ]'
		rule: #methodBody
		equals: ' method body with "''", ''"'', "[", '']'''.
	
]

{ #category : #tests }
TonelParserTest >> testMethodBodyWithCommentNextToDollarCharLiteral [
	self 
        assertParse: '[^$$"what about this one]"]'
        rule: #methodBody
        equals: '^$$"what about this one]"'
]

{ #category : #tests }
TonelParserTest >> testMethodBodyWithSquareBracketInsideLiteralArray [
    self 
        assertParse: '[ "What about this valid literal Array guys" ^#( [ )]'
        rule: #methodBody
        equals: ' "What about this valid literal Array guys" ^#( [ )'.
]

{ #category : #tests }
TonelParserTest >> testMethodDef [
	self 
		assertParse: '
{ #category: ''accessing'' }
Object>>name [
	^ self printString
]'
		rule: #methodDef
		equals: (MCMethodDefinition
			className: #Object
			classIsMeta: false
			selector: #name
			category: 'accessing' 
			timeStamp: nil
			source: 'name
	^ self printString').
			
	self 
		assertParse: '
Object class>>name [ 
	^ self printString
]'
		rule: #methodDef
		equals: (MCMethodDefinition
			className: #Object
			classIsMeta: true
			selector: #name
			category: ''
			timeStamp: nil
			source: 'name 
	^ self printString').

	self 
		assertParse: '
TClass classSide >> template: aSystemCategoryName [ 
	"I really do not care"
]'
		rule: #methodDef
		equals: (MCMethodDefinition
			className: #TClass
			classIsMeta: true
			selector: #template:
			category: ''
			timeStamp: nil
			source: 'template: aSystemCategoryName 
	"I really do not care"').

]

{ #category : #tests }
TonelParserTest >> testMethodDefList [
	| parsed |
	
	parsed := self 
		parse: '
Object class>>new [
	^ self basicNew initialize
]

{ #category: ''accessing'' }
Object>>name [
	^ self printString
]

{ #category: ''printing'' }
Object>>printOn: aStream [
	"Append to the argument, aStream, a sequence of characters that  
	identifies the receiver."

	| title |
	title := self class name.
	aStream
		nextPutAll: (title first isVowel ifTrue: [''an ''] ifFalse: [''a '']);
		nextPutAll: title

]'
		rule: #methodDefList.
		
	self assert: parsed size equals: 3

]

{ #category : #tests }
TonelParserTest >> testType [
	self assertParse: 'Class' rule: #type equals: 'Class'.
	self assertParse: 'Trait' rule: #type equals: 'Trait'.
	self assertParse: 'Extension' rule: #type equals: 'Extension'
]

{ #category : #tests }
TonelParserTest >> testTypeDef [
	self 
		assertParse: '
"
this is a test
"
Class { 
	#name: ''X'',
	#superclass: ''Y'',
	#category: ''Z'' 
}'
		rule: #typeDef
		equals: (MCClassDefinition
        name: 'X'
        superclassName: 'Y'
        traitComposition: '{}'
        classTraitComposition: '{}'
        category: 'Z'
        instVarNames: #()
        classVarNames: #()
        poolDictionaryNames: #()
        classInstVarNames: #()
        type: #normal
        comment: 'this is a test'
        commentStamp: nil)
]
