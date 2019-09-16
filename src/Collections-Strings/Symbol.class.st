"
I represent Strings that are created uniquely. Thus, someString asSymbol == someString asSymbol.

To see the difference between Symbol and Strings look at this example:

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

""Comparing Symbols takes less time than comparing Strings""

[ #stringA = #stringB ] bench. ""26,812,864 per second""
[ 'StringA' = 'StringB' ] bench. ""3,492,987 per second""
"
Class {
	#name : #Symbol,
	#superclass : #String,
	#classVars : [
		'NewSymbols',
		'OneCharacterSymbols',
		'SelectorTable',
		'SymbolTable'
	],
	#category : #'Collections-Strings-Base'
}

{ #category : #initialization }
Symbol class >> allSymbolTablesDo: aBlock [

	NewSymbols do: aBlock.
	SymbolTable do: aBlock.
]

{ #category : #initialization }
Symbol class >> allSymbolTablesDo: aBlock after: aSymbol [

	NewSymbols do: aBlock after: aSymbol.
	SymbolTable do: aBlock after: aSymbol.
]

{ #category : #access }
Symbol class >> allSymbols [
	"Answer all interned symbols"
	^Array streamContents:[:s|
		s nextPutAll: NewSymbols.
		s nextPutAll: OneCharacterSymbols.
		s nextPutAll: SymbolTable.
	].

]

{ #category : #cleanup }
Symbol class >> cleanUp [
	"Flush caches"

	self compactSymbolTable.
	self rebuildSelectorTable
]

{ #category : #initialization }
Symbol class >> compactSymbolTable [
	"Reduce the size of the symbol table so that it holds all existing symbols + 25% (changed from 1000 since sets like to have 25% free and the extra space would grow back in a hurry)"

	| oldSize |

	Smalltalk garbageCollect.
	oldSize := SymbolTable array size.
	SymbolTable growTo: SymbolTable size * 4 // 3 + 100.
	^oldSize printString,'  ',(oldSize - SymbolTable array size) printString, ' slot(s) reclaimed'
]

{ #category : #'instance creation' }
Symbol class >> findInterned: aString [

	self hasInterned:aString ifTrue:[:symbol| ^symbol].
	^nil.
]

{ #category : #'instance creation' }
Symbol class >> findInternedSelector: aString [
	| symbol |
	symbol := self findInterned: aString.
	"if it isn't found or not a SelectorSymbol - return nil"
	(symbol isNil or: [ symbol isSelectorSymbol not ])
		ifTrue: [ ^ nil ].
	"otherwise, return this symbol"
	^ symbol
]

{ #category : #private }
Symbol class >> hasInterned: aString ifTrue: symBlock [
	"Answer with false if aString hasnt been interned (into a Symbol),  
	otherwise supply the symbol to symBlock and return true."

	^ (self lookup: aString)
		ifNil: [ false ]
		ifNotNil: [ :symbol | 
			symBlock value: symbol.
			true ]
]

{ #category : #private }
Symbol class >> initSelectorTable [
	^ SelectorTable := WeakSet new
]

{ #category : #initialization }
Symbol class >> initialize [
	Symbol rehash.
	OneCharacterSymbols := nil.
	OneCharacterSymbols := (1 to: 256) collect: [ :i | (i - 1) asCharacter asSymbol].

	SessionManager default
		registerSystemClassNamed: #Symbol
]

{ #category : #'instance creation' }
Symbol class >> intern: aStringOrSymbol [ 

	^(self lookup: aStringOrSymbol) ifNil:[
		| aClass aSymbol |
		aStringOrSymbol isSymbol ifTrue:[
			aSymbol := aStringOrSymbol.
		] ifFalse:[
			aClass := aStringOrSymbol isOctetString ifTrue:[ByteSymbol] ifFalse:[WideSymbol].
			aSymbol := aClass basicNew: aStringOrSymbol size.
			aSymbol string: aStringOrSymbol.
		].
		NewSymbols add: aSymbol.
		aSymbol].
]

{ #category : #'instance creation' }
Symbol class >> internCharacter: aCharacter [
	aCharacter asciiValue > 256 ifTrue:[^self intern: aCharacter asString].
	OneCharacterSymbols ifNil: [^self intern: aCharacter asString].
	^OneCharacterSymbols at: aCharacter asciiValue + 1

]

{ #category : #'instance creation' }
Symbol class >> internSelector: aStringOrSymbol [
	| selector |
	selector := (self selectorTable like: aStringOrSymbol)
		ifNil: [ self selectorTable add: aStringOrSymbol asSymbol ].
	^ selector
]

{ #category : #'instance creation' }
Symbol class >> lookup: aStringOrSymbol [

	^(SymbolTable like: aStringOrSymbol) ifNil: [
		NewSymbols like: aStringOrSymbol
	]
]

{ #category : #'instance creation' }
Symbol class >> new: aSize [

	self shouldNotImplement .
]

{ #category : #'stream creation' }
Symbol class >> new: size streamContents: aBlock [
	^ (super new: size streamContents: aBlock) asSymbol
]

{ #category : #'instance creation' }
Symbol class >> newFrom: aCollection [ 
	"Answer an instance of me containing the same elements as aCollection."

	^ (aCollection as: String) asSymbol

"	Symbol newFrom: {$P. $e. $n}
	{$P. $e. $n} as: Symbol
"
]

{ #category : #private }
Symbol class >> possibleSelectorsFor: misspelled [ 
	"Answer an ordered collection of possible corrections
	for the misspelled selector in order of likelyhood"

	| numArgs candidates lookupString best binary short long first |
	lookupString := misspelled asLowercase. "correct uppercase selectors to lowercase"
	numArgs := lookupString numArgs.
	(numArgs < 0 or: [lookupString size < 2]) ifTrue: [^ OrderedCollection new: 0].
	first := lookupString first.
	short := lookupString size - (lookupString size // 4 max: 3) max: 2.
	long := lookupString size + (lookupString size // 4 max: 3).

	"First assemble candidates for detailed scoring"
	candidates := OrderedCollection new.
	self allSymbolTablesDo: [:s | | ss | (((ss := s size) >= short	"not too short"
			and: [ss <= long			"not too long"
					or: [(s at: 1) = first]])	"well, any length OK if starts w/same letter"
			and: [s numArgs = numArgs])	"and numArgs is the same"
			ifTrue: [candidates add: s]].

	"Then further prune these by correctAgainst:"
	best := lookupString correctAgainst: candidates.
	((misspelled last ~~ $:) and: [misspelled size > 1]) ifTrue: [
		binary := misspelled, ':'.		"try for missing colon"
		Symbol hasInterned: binary ifTrue: [:him | best addFirst: him]].
	^ best
]

{ #category : #'instance creation' }
Symbol class >> readFrom: strm [  
	"Symbol readFromString: '#abc'"

	strm peek = $# ifFalse: [self error: 'Symbols must be introduced by #'].
    	^ strm contents parseLiterals first.
 
]

{ #category : #cleanup }
Symbol class >> rebuildSelectorTable [
	self initSelectorTable.
	CompiledMethod
		allInstancesDo: [ :method | 
			| selector |
			selector := method selector.
			selector ifNotNil: [ SelectorTable add: selector ] ]
]

{ #category : #private }
Symbol class >> rehash [
   "Symbol rehash"
	"Rebuild the hash table, reclaiming unreferenced Symbols."

	SymbolTable := WeakSet withAll: self allSubInstances.
	NewSymbols := WeakSet new.
	self rebuildSelectorTable.
]

{ #category : #private }
Symbol class >> selectorTable [
	^SelectorTable ifNil: [self initSelectorTable]
]

{ #category : #accessing }
Symbol class >> selectorThatStartsCaseSensitive: leadingCharacters skipping: skipSym [
	"Same as thatStartsCaseSensitive:skipping: but on the SelectorTable only"
	| size firstMatch key |

	size := leadingCharacters size.
	size = 0 ifTrue: [^skipSym ifNil: [#''] ifNotNil: [nil]].
	firstMatch := leadingCharacters at: 1.
	size > 1 ifTrue: [key := leadingCharacters copyFrom: 2 to: size].
	self selectorTable do: [:each |
			(each notNil and: [each size >= size]) ifTrue:
				[
					((each at: 1) == firstMatch and:
						[key == nil or:
							[(each findString: key startingAt: 2 caseSensitive: true) = 2]])
								ifTrue: [^each]
				]
		] after: skipSym.

	^nil

]

{ #category : #access }
Symbol class >> selectorsContaining: aString [
	"Answer a list of selectors that contain aString within them. Case-insensitive.  Does return symbols that begin with a capital letter."

	| size selectorList ascii |

	selectorList := OrderedCollection new.
	(size := aString size) = 0 ifTrue: [^selectorList].

	aString size = 1 ifTrue:
		[
			ascii := aString first asciiValue.
			ascii < 128 ifTrue: [selectorList add: (OneCharacterSymbols at: ascii+1)]
		].

	(aString first isAlphaNumeric) ifFalse:
		[
			aString size = 2 ifTrue: 
				[Symbol hasInterned: aString ifTrue:
					[:s | selectorList add: s]].
			^selectorList
		].

	selectorList := selectorList copyFrom: 2 to: selectorList size.

	self allSymbolTablesDo: [:each |
		each size >= size ifTrue:
			[(each findSubstring: aString in: each startingAt: 1 
				matchTable: CaseInsensitiveOrder) > 0
						ifTrue: [selectorList add: each]]].

	^selectorList reject: [:each | "reject non-selectors, but keep ones that begin with an uppercase"
		each numArgs < 0 and: [each asString uncapitalized numArgs < 0]].

"Symbol selectorsContaining: 'scon'"
]

{ #category : #private }
Symbol class >> shutDown: aboutToQuit [

	SymbolTable addAll: NewSymbols.
	NewSymbols := WeakSet new.

]

{ #category : #accessing }
Symbol class >> streamSpecies [
	^ String
]

{ #category : #access }
Symbol class >> thatStarts: leadingCharacters skipping: skipSym [
	"Answer a selector symbol that starts with leadingCharacters.
	Symbols beginning with a lower-case letter handled directly here.
	Ignore case after first char.
	If skipSym is not nil, it is a previous answer; start searching after it.
	If no symbols are found, answer nil.
	Used by Alt-q (Command-q) routines"

	| size firstMatch key |

	size := leadingCharacters size.
	size = 0 ifTrue: [^skipSym ifNil: [#''] ifNotNil: [nil]].

	firstMatch := leadingCharacters at: 1.
	size > 1 ifTrue: [key := leadingCharacters copyFrom: 2 to: size].

	self allSymbolTablesDo: [:each |
			each size >= size ifTrue:
				[
					((each at: 1) == firstMatch and:
						[key == nil or:
							[(each findString: key startingAt: 2 caseSensitive: false) = 2]])
								ifTrue: [^each]
				]
		] after: skipSym.

	^nil

"Symbol thatStarts: 'sf' skipping: nil"
"Symbol thatStarts: 'sf' skipping: #sfpGetFile:with:with:with:with:with:with:with:with:"
"Symbol thatStarts: 'candidate' skipping: nil"

]

{ #category : #access }
Symbol class >> thatStartsCaseSensitive: leadingCharacters skipping: skipSym [
	"Same as thatStarts:skipping: but caseSensitive"
	| size firstMatch key |

	size := leadingCharacters size.
	size = 0 ifTrue: [^skipSym ifNil: [#''] ifNotNil: [nil]].
	firstMatch := leadingCharacters at: 1.
	size > 1 ifTrue: [key := leadingCharacters copyFrom: 2 to: size].
	self allSymbolTablesDo: [:each |
			each size >= size ifTrue:
				[
					((each at: 1) == firstMatch and:
						[key == nil or:
							[(each findString: key startingAt: 2 caseSensitive: true) = 2]])
								ifTrue: [^each]
				]
		] after: skipSym.

	^nil

]

{ #category : #'instance creation' }
Symbol class >> with: aCharacter [
	^self newFrom: aCharacter asOrderedCollection
]

{ #category : #'instance creation' }
Symbol class >> withAll: aCollection [
	^ self newFrom: aCollection
]

{ #category : #copying }
Symbol >> , otherSymbolOrString [
	"Concatenate the receiver with otherSymbolOrString and return the result.
	Overwritten to always return Symbol results,
	whether the argument is a Symbol or a String"
	
	^ (super , otherSymbolOrString) asSymbol
]

{ #category : #comparing }
Symbol >> = aSymbol [
	"Compare the receiver and aSymbol." 
	self == aSymbol ifTrue: [^ true].
	self class == aSymbol class ifTrue: [^ false].
	"Use String comparison otherwise"
	^ super = aSymbol
]

{ #category : #converting }
Symbol >> asAnnouncement [

	^ self
]

{ #category : #converting }
Symbol >> asMutator [
	"Return a setter message from a getter message.
	Return self if it is already a setter. 
	Pay attention the implementation should be improved to return valid selector."
	
	"#name asMutator >>> #name:"
	"#name: asMutator >>> #name:"

	"#_ asMutator >>> #_:"
	"#foo:: asMutator >>> #'foo::'"
	
	self endsWithAColon ifTrue:[ ^ self ].
	^ (self copyWith: $:) asSymbol
]

{ #category : #converting }
Symbol >> asString [ 
	"Refer to the comment in String|asString."
	| newString |
	newString := self species new: self size.
	newString replaceFrom: 1 to: newString size with: self startingAt: 1.
	^newString
]

{ #category : #converting }
Symbol >> asSymbol [ 
	"Refer to the comment in String|asSymbol."
]

{ #category : #accessing }
Symbol >> at: anInteger put: anObject [ 
	"You cannot modify the receiver."

	self errorNoModification
]

{ #category : #converting }
Symbol >> capitalized [
	^ self asString capitalized asSymbol
]

{ #category : #copying }
Symbol >> copy [
	"Answer with the receiver, because Symbols are unique."
]

{ #category : #evaluating }
Symbol >> cull: anObject [ 
	^anObject perform: self.
]

{ #category : #private }
Symbol >> errorNoModification [

	self error: 'symbols can not be modified.'
]

{ #category : #'system primitives' }
Symbol >> flushCache [
	"Tell the virtual machine to remove all entries with this symbol as a selector from its method lookup caches, if it has any.  This 	must be done whenever a method is added, redefined or removed, so that message lookups reflect the revised organization.  c.f. 	Behavior>>flushCache & CompiledMethod>>flushCache.  Essential. See MethodDictionary class comment."

	<primitive: 119>

]

{ #category : #announcements }
Symbol >> handlesAnnouncement: anAnnouncement [ 
	"Anything else than the announcement identifier (in the case of symbol i.e. #foo for ... on: #foo send: #bar to: nil) will not 
	be handled."
	
	"Announcer new 
		on: #FOO send: #bar to: nil;
		announce: #FOO should raise DNU bar"
	
	^ anAnnouncement == self
]

{ #category : #testing }
Symbol >> includesKey: sym [
	^self == sym.
]

{ #category : #testing }
Symbol >> isBinary [
	"Answer whether the receiver is a binary message selector."

	^ self precedence = 2
]

{ #category : #testing }
Symbol >> isDoIt [

	^ (self == #DoIt) or: [self == #DoItIn:].
]

{ #category : #testing }
Symbol >> isInfix [
	"Answer whether the receiver is an infix message selector."

	^ self precedence = 2
]

{ #category : #testing }
Symbol >> isKeyword [
	"Answer whether the receiver is a message keyword."

	^ self precedence = 3
]

{ #category : #printing }
Symbol >> isOrientedFill [
	"Needs to be implemented here because symbols can occupy 'color' slots of morphs."

	^ false
]

{ #category : #testing }
Symbol >> isSelectorSymbol [
	^ (self class selectorTable like: self) notNil
]

{ #category : #testing }
Symbol >> isSymbol [
	^ true 
]

{ #category : #testing }
Symbol >> isUnary [
	"Answer whether the receiver is an unary message selector."

	^ self precedence = 1
]

{ #category : #'system primitives' }
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
			stream nextPutAll: 'with:' ]].
	

]

{ #category : #accessing }
Symbol >> precedence [
	"Answer the receiver's precedence, assuming it is a valid Smalltalk
	message selector or 0 otherwise.  The numbers are 1 for unary,
	2 for binary and 3 for keyword selectors."

	self size = 0 ifTrue: [^ 0].
	(self first isLetter or: [ self first = $_ ]) ifFalse: [^ 2].
	self last = $: ifTrue: [^ 3].
	^ 1
]

{ #category : #announcements }
Symbol >> prepareForDelivery [
	"somehow symbol can be announcemnt"
]

{ #category : #accessing }
Symbol >> replaceFrom: start to: stop with: replacement startingAt: repStart [

	self errorNoModification
]

{ #category : #converting }
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

{ #category : #copying }
Symbol >> shallowCopy [
	"Answer with the receiver, because Symbols are unique."
]

{ #category : #sorting }
Symbol >> sorted: aSortBlockOrNil [
	"Return a new sequenceable collection which contains the same elements as self but its elements are sorted by aSortBlockOrNil. The block should take two arguments and return true if the first element should preceed the second one. If aSortBlock is nil then <= is used for comparison. We convert the symbol to an array because symbols can't be changed."
	
	^self asArray sort: aSortBlockOrNil
]

{ #category : #printing }
Symbol >> storeOn: aStream [ 

	aStream nextPut: $#.
	(self isLiteralSymbol)
		ifTrue: [aStream nextPutAll: self]
		ifFalse: [super storeOn: aStream]
]

{ #category : #private }
Symbol >> string: aString [

	1 to: aString size do: [:j | super at: j put: (aString at: j)].
	^self  
]

{ #category : #converting }
Symbol >> uncapitalized [
	"Answer an object like the receiver but with first character downshifted if necessary"

	^ self asString uncapitalized asSymbol
]

{ #category : #evaluating }
Symbol >> value: anObject [ 
	^anObject perform: self.
]

{ #category : #copying }
Symbol >> veryDeepCopyWith: deepCopier [
	"Return self.  I am immutable in the Morphic world.  Do not record me."
]
