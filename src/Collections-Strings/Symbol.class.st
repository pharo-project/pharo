"
I represent Strings that are created uniquely. Thus, someString asSymbol == someString asSymbol.

To see the difference between Symbol and Strings look at this example:

```
| s1 s2 |
s1 := 1234 asString.
s2 := 1234 asString.

""Strings are not unique, while Symbols are""
s1 = s2. ""true""
s1 == s2. ""false""

s1 asSymbol = s2 asSymbol. ""true""
s1 asSymbol == s2 asSymbol. ""true""

(s1 class allInstances select: [:s | s = s1 ]) size. ""2""
(s1 asSymbol class allInstances select: [:s | s = s1 asSymbol ]) size. ""1""
```

Comparing Symbols takes less time than comparing Strings.

```
[ #stringA = #stringB ] bench. ""26,812,864 per second""
[ 'StringA' = 'StringB' ] bench. ""3,492,987 per second""
```
"
Class {
	#name : 'Symbol',
	#superclass : 'String',
	#category : 'Collections-Strings-Base',
	#package : 'Collections-Strings',
	#tag : 'Base'
}

{ #category : 'symbol table' }
Symbol class >> allSymbolTablesDo: aBlock [
	
	SymbolTable system allSymbolsDo: aBlock
]

{ #category : 'accessing' }
Symbol class >> allSymbols [
	"Answer all interned symbols"

	^ Array streamContents: [ :s | self allSymbolTablesDo: [ :symbol | s nextPut: symbol ] ]
]

{ #category : 'cleanup' }
Symbol class >> cleanUp [
	"Flush caches"

	 SymbolTable system rehash
]

{ #category : 'symbol table' }
Symbol class >> findInterned: aStringOrSymbol [
 
	^ SymbolTable system findInterned: aStringOrSymbol
]

{ #category : 'symbol table' }
Symbol class >> hasInterned: aStringOrSymbol [
	"Answer with false if aString hasnt been interned (into a Symbol)"

	^ (self findInterned: aStringOrSymbol) notNil
]

{ #category : 'symbol table' }
Symbol class >> hasInterned: aString ifTrue: symBlock [
	"Answer with false if aString hasnt been interned (into a Symbol),
	otherwise supply the symbol to symBlock and return true.
	Consider using #findInterned: with #ifNotNil: instead"

	^ (self findInterned: aString)
		ifNil: [ false ]
		ifNotNil: [ :symbol |
			symBlock value: symbol.
			true ]
]

{ #category : 'symbol table' }
Symbol class >> intern: aStringOrSymbol [

	^ SymbolTable system intern: aStringOrSymbol
]

{ #category : 'instance creation' }
Symbol class >> new: aSize [

	self shouldNotImplement
]

{ #category : 'instance creation' }
Symbol class >> newFrom: aCollection [
	"Answer an instance of me containing the same elements as aCollection."

	^ (aCollection as: String) asSymbol

"	Symbol newFrom: {$P. $e. $n}
	{$P. $e. $n} as: Symbol
"
]

{ #category : 'cleanup' }
Symbol class >> rehash [

	 SymbolTable system rehash
]

{ #category : 'accessing' }
Symbol class >> reservedLiterals [

	^ PseudoVariable lookupDictionary keys union: #( #nil #true #false )
]

{ #category : 'system startup' }
Symbol class >> shutDown: aboutToQuit [

	SymbolTable system compactSymbols
]

{ #category : 'accessing' }
Symbol class >> streamSpecies [
	^ String
]

{ #category : 'instance creation' }
Symbol class >> with: aCharacter [

	^ self newFrom: (OrderedCollection with: aCharacter)
]

{ #category : 'instance creation' }
Symbol class >> withAll: aCollection [
	^ self newFrom: aCollection
]

{ #category : 'copying' }
Symbol >> , otherSymbolOrString [
	"Concatenate the receiver with otherSymbolOrString and return the result.
	Overwritten to always return Symbol results,
	whether the argument is a Symbol or a String"

	^ (super , otherSymbolOrString) asSymbol
]

{ #category : 'comparing' }
Symbol >> = aSymbol [
	"Compare the receiver and aSymbol."
	self == aSymbol ifTrue: [^ true].
	self class == aSymbol class ifTrue: [^ false].
	"Use String comparison otherwise"
	^ super = aSymbol
]

{ #category : 'evaluating' }
Symbol >> argumentCount [
    "This method refers to block arguments [:acc :x | ...] has two arguments so argumentCount of a selector that could be used in place of a block
	should return the same amount of argument than the block."
	^ self numArgs + 1
]

{ #category : 'converting' }
Symbol >> asAccessor [
	"Return a accessor (getter) message from a setter message.
	Return self if it is already a getter."

	"#name asAccessor >>> #name"
	"#name: asAccessor >>> #name"
	
	(((self count: [ :char | char = $: ]) > 1) 
		or: [ self isEmpty 
		or: [ self isBinary ]]) ifTrue: [ 
		self error: 'This symbol cannot be converted to an accessor' ].

	self endsWithAColon ifFalse:[ ^ self ].
	^ (self withoutSuffix: ':') asSymbol
]

{ #category : 'converting' }
Symbol >> asMethodSignature [
	"Returns the method signature method name with parameters based on the receiving selector."
	
	self numArgs = 0
		ifTrue: [ ^ self asString ].
	^ String
		streamContents: [ :str |
			| keywords |
			keywords := self keywords.
			keywords
				doWithIndex: [ :each :index |
					str
						nextPutAll: each;
						nextPutAll: ' var';
						nextPutAll: index asString.
					index = keywords size ifFalse: [ str space ] ] ]
]

{ #category : 'converting' }
Symbol >> asMutator [
	"Return a setter message from a getter message.
	Return self if it is already a setter.
	Pay attention the implementation should be improved to return valid selector."

	"#name asMutator >>> #name:"
	"#name: asMutator >>> #name:"

	"#_ asMutator >>> #_:"
	"#foo:: asMutator >>> #'foo::'"
	
	self isBinary ifTrue: [ self error: 'Binary selectors cannot be transformed into mutators' ].

	self endsWithAColon ifTrue:[ ^ self ].
	^ (self copyWith: $:) asSymbol
]

{ #category : 'converting' }
Symbol >> asString [
	"Refer to the comment in String|asString."
	| newString |
	newString := self species new: self size.
	newString replaceFrom: 1 to: newString size with: self startingAt: 1.
	^newString
]

{ #category : 'converting' }
Symbol >> asSymbol [
	"Refer to the comment in String|asSymbol."
]

{ #category : 'accessing' }
Symbol >> at: anInteger put: anObject [
	"You cannot modify the receiver."

	self errorNoModification
]

{ #category : 'converting' }
Symbol >> capitalized [
	^ self asString capitalized asSymbol
]

{ #category : 'copying' }
Symbol >> copy [
	"Answer with the receiver, because Symbols are unique."
	^ self
]

{ #category : 'converting' }
Symbol >> createSymbol [

	^ self
]

{ #category : 'evaluating' }
Symbol >> cull: anObject [
	^anObject perform: self
]

{ #category : 'private' }
Symbol >> errorNoModification [

	self error: 'symbols can not be modified.'
]

{ #category : 'system primitives' }
Symbol >> flushCache [
	"Tell the virtual machine to remove all entries with this symbol as a selector from its method lookup caches, if it has any.  This 	must be done whenever a method is added, redefined or removed, so that message lookups reflect the revised organization.  c.f. 	Behavior>>flushCache & CompiledMethod>>flushCache.  Essential. See MethodDictionary class comment."

	<primitive: 119>
]

{ #category : 'converting' }
Symbol >> handleDoesNotUnderstand: aMessage to: aReceiver [

	<debuggerCompleteToSender>
	| exception resumeValue |
	(exception := MessageNotUnderstood new)
		message: aMessage;
		receiver: aReceiver.
	resumeValue := exception signal.
	^exception hasToResend
		ifTrue: [aMessage sentTo: aReceiver]
		ifFalse: [resumeValue]
]

{ #category : 'announcements' }
Symbol >> handlesAnnouncement: anAnnouncement [
	"Anything else than the announcement identifier (in the case of symbol i.e. #foo for ... on: #foo send: #bar to: nil) will not
	be handled."

	"Announcer new
		on: #FOO send: #bar to: nil;
		announce: #FOO should raise DNU bar"

	^ anAnnouncement == self
]

{ #category : 'testing' }
Symbol >> includesKey: sym [
	^self == sym
]

{ #category : 'testing' }
Symbol >> isBinary [
	"Answer whether the receiver is a binary message selector."

	^ self precedence = 2
]

{ #category : 'testing' }
Symbol >> isDoIt [

	^ self == #DoIt
]

{ #category : 'testing' }
Symbol >> isInfix [
	"Answer whether the receiver is an infix message selector."

	^ self precedence = 2
]

{ #category : 'testing' }
Symbol >> isKeyword [
	"Answer whether the receiver is a message keyword."

	^ self precedence = 3
]

{ #category : 'testing' }
Symbol >> isOrientedFill [
	"Needs to be implemented here because symbols can occupy 'color' slots of morphs."

	^ false
]

{ #category : 'testing' }
Symbol >> isScopedSelector [

	^ false
]

{ #category : 'testing' }
Symbol >> isSymbol [
	^ true
]

{ #category : 'testing' }
Symbol >> isUnary [
	"Answer whether the receiver is an unary message selector."

	"#new isUnary >>> true"
	"#>> isUnary >>> false"
	"#between:and: isUnary >>> false"

	^ self precedence = 1
]

{ #category : 'testing' }
Symbol >> isUsedAsSelectorSymbol [
	"Returns whether the receiver is currently used as a method selector.
	Notice that a symbol representing a valid method selector could return false if there are no method installed with such selector."
	
	^ SymbolTable system selectorTable includes: self
]

{ #category : 'sorting' }
Symbol >> keywordsStrict [
	"Returns the keywords of the provided selector. If the receiver is not keyword-based, an empty array is returned."

	"#foo: keywordsStrict >>> #('foo:')"

	"#foo:bar: keywordsStrict >>> #('foo:' 'bar:')"

	"#foo keywordsStrict >>> #()"

	"#+ keywordsStrict >>> #()"

	^ self isKeyword
		  ifTrue: [ self keywords ]
		  ifFalse: [ #(  ) ]
]

{ #category : 'comparing' }
Symbol >> literalEqual: other [

	^ self == other
]

{ #category : 'system primitives' }
Symbol >> numArgs: n [
	"Answer a string that can be used as a selector with n arguments.
	 TODO: need to be extended to support shrinking and for selectors like #+ "

	| selector numArgs offs |

	selector := self.
	(numArgs := selector numArgs) >= n
		ifTrue: [ ^ self ].

	^ self class new: 16 streamContents: [ :stream|
		stream nextPutAll: self.
		(numArgs = 0)
			ifTrue: [ stream nextPut: $:. offs := 0]
			ifFalse: [ offs := 1 ].
		2 to: n - numArgs + offs do: [:i |
			stream nextPutAll: 'with:' ]]
]

{ #category : 'accessing' }
Symbol >> precedence [
	"Answer the receiver's precedence, assuming it is a valid Smalltalk
	message selector or 0 otherwise.  The numbers are 1 for unary,
	2 for binary and 3 for keyword selectors."

	self size = 0 ifTrue: [^ 0].
	(self first isLetter or: [ self first = $_ ]) ifFalse: [
		| special |
		special := Character specialCharacters.
		^ (self allSatisfy: [ :char | special includes: char ])
			ifTrue: [ 2 ]
			ifFalse: [ 0 ] ].
	self last = $: ifTrue: [^ 3].
	^ 1
]

{ #category : 'announcements' }
Symbol >> prepareForDelivery [
	"somehow symbol can be announcemnt"
]

{ #category : 'accessing' }
Symbol >> replaceFrom: start to: stop with: replacement startingAt: repStart [

	self errorNoModification
]

{ #category : 'converting' }
Symbol >> separateKeywords [
	"#'foo:zork:' separateKeywords -> 'foo:  zork:'"

	self isKeyword
		ifFalse: [ ^ self ].
	^ String streamContents: [ :stream |
			(self findTokens: $:)
				do: [ :each |
					stream
						nextPutAll: each;
						nextPut: $: ]
				separatedBy: [ stream nextPutAll: '  ' ] ]
]

{ #category : 'copying' }
Symbol >> shallowCopy [
	"Answer with the receiver, because Symbols are unique."
]

{ #category : 'sorting' }
Symbol >> sorted: aSortBlockOrNil [
	"Return a new sequenceable collection which contains the same elements as self but its elements are sorted by aSortBlockOrNil. The block should take two arguments and return true if the first element should preceed the second one. If aSortBlock is nil then <= is used for comparison. We convert the symbol to an array because symbols can't be changed."

	^self asArray sort: aSortBlockOrNil
]

{ #category : 'storing' }
Symbol >> storeOn: aStream [

	aStream nextPut: $#.
	(self isLiteralSymbol)
		ifTrue: [aStream nextPutAll: self]
		ifFalse: [super storeOn: aStream]
]

{ #category : 'private' }
Symbol >> string: aString [

	1 to: aString size do: [:j | super at: j put: (aString at: j)].
	^self
]

{ #category : 'converting' }
Symbol >> uncapitalized [
	"Answer an object like the receiver but with first character downshifted if necessary"

	^ self asString uncapitalized asSymbol
]

{ #category : 'evaluating' }
Symbol >> value: anObject [
	^anObject perform: self
]

{ #category : 'evaluating' }
Symbol >> value: anObject value: arg [

	^ anObject perform: self with: arg
]

{ #category : 'evaluating' }
Symbol >> valueWithArguments: aCollection [

	^ aCollection first perform: self withArguments: aCollection allButFirst
]

{ #category : 'evaluating' }
Symbol >> valueWithEnoughArguments: anArray [
	"call me with enough arguments from anArray"
	<reflection: 'Message sending and code execution - Message send reification'>
	| args |
	(anArray size == self argumentCount)
		ifTrue: [ ^self valueWithArguments: anArray ].

	args := Array new: self argumentCount.
	args replaceFrom: 1
		to: (anArray size min: args size)
		with: anArray
		startingAt: 1.

	^ self valueWithArguments: args
]
