"
Parse mail addresses.  The basic syntax is:

	addressList := MailAddressParser addressesIn: aString

This currently only returns the bare addresses, but it could also return a list of the address ""source codes"".  For example, if you give it ""Joe <joe@foo>, <jane>"", it will currently return a list ('joe@foo' 'jane').  It would be nice to also get a list ('Joe <joe@foo>'  '<jane>').
"
Class {
	#name : #MailAddressParser,
	#superclass : #Object,
	#instVars : [
		'tokens',
		'addresses',
		'curAddrTokens',
		'storeNames'
	],
	#category : #'Network-Mail'
}

{ #category : #parsing }
MailAddressParser class >> addressesAndNamePairsIn: aString [
	"return a collection of the addresses and the corresponding names listed in aString"
	| tokens |
	tokens := MailAddressTokenizer tokensIn: aString.
	^(self new initialize: tokens) grabAddressesAndNames
]

{ #category : #parsing }
MailAddressParser class >> addressesIn: aString [
	"return a collection of the bare addresses listed in aString"
	| tokens |
	tokens := MailAddressTokenizer tokensIn: aString.
	^(self new initialize: tokens) grabAddresses
]

{ #category : #'building address list' }
MailAddressParser >> addToAddress [
	"add the last token to the address.  removes the token from the collection"
	curAddrTokens addFirst: (tokens removeLast)
]

{ #category : #'building address list' }
MailAddressParser >> finishAddress [
	"we've finished one address.  Bundle it up and add it to the list of addresses"
	| address |

	address := String streamContents: [ :str |
		curAddrTokens do: [ :tok | str nextPutAll: tok text ] ].

	addresses addFirst: address.

	curAddrTokens := nil.
]

{ #category : #parsing }
MailAddressParser >> grabAddressWithRoute [
	"grab an address of the form 'Descriptive Text <real.address@c.d.e>"

	| name |
	
	self startNewAddress.

	tokens removeLast.	"remove the >"

	"grab until we see a $<"
	[ 
		tokens isEmpty ifTrue: [
			self error: '<> are not matched' ].
		tokens last type = $<
	] whileFalse: [ self addToAddress ].

	tokens removeLast.  "remove the <"
	self finishAddress.
	name := self grabName.
	storeNames ifTrue: [addresses addFirst: {name . addresses removeFirst}].
]

{ #category : #parsing }
MailAddressParser >> grabAddresses [
	"grab all the addresses in the string"
	| token |
	"remove comments"
	tokens removeAllSuchThat: [:t | t type == #Comment].
	"grab one address or address group each time through this loop"
	[ 
		"remove commas"
		[
			tokens isEmpty not and: [ tokens last type = $, ]
		] whileTrue: [ tokens removeLast ].

		"check whether any tokens are left"
		tokens isEmpty 
	] whileFalse: [
		token := tokens last.

		"delegate, depending on what form the address is in"
		"the from can be determined from the last token"

		token type = $> ifTrue: [
			self grabAddressWithRoute ]
		ifFalse: [ 
			(#(Atom DomainLiteral QuotedString) includes: token type)  ifTrue: [
				self grabBasicAddress ]
		ifFalse: [
			token type = $; ifTrue: [
				self grabGroupAddress ]
		ifFalse: [
			^self error: 'un-recognized address format' ] ] ]
	].

	^addresses
]

{ #category : #parsing }
MailAddressParser >> grabAddressesAndNames [
	
	storeNames := true.
	
	self grabAddresses.

	addresses := addresses collect: [:a |
		a isString 
			ifTrue: [{'' . a}]
			ifFalse: [a] ].

	^ addresses
]

{ #category : #parsing }
MailAddressParser >> grabBasicAddress [
	"grad an address of the form a.b@c.d.e"
	self startNewAddress.
	"grab either the domain if specified, or the domain if not"
	self addToAddress.
	[tokens isEmpty not and: [ tokens last type = $.] ] 
		whileTrue: 
			["add name-dot pairs of tokens"
			self addToAddress.
			(#(Atom QuotedString ) includes: tokens last type)
				ifFalse: [self error: 'bad token in address: ' , tokens last text].
			self addToAddress].
	(tokens isEmpty or: [tokens last type ~= $@])
		ifTrue: ["no domain specified"
			self finishAddress]
		ifFalse: 
			["that was the domain.  check that no QuotedString's slipped in"
			curAddrTokens do: [:tok | tok type = #QuotedString ifTrue: [self error: 'quote marks are not allowed within a domain name (' , tok text , ')']].
			"add the @ sign"
			self addToAddress.
			"add the local part"
			(#(Atom QuotedString ) includes: tokens last type)
				ifFalse: [self error: 'invalid local part for address: ' , tokens last text].
			self addToAddress.
			"add word-dot pairs if there are any"
			[tokens isEmpty not and: [tokens last type = $.]]
				whileTrue: 
					[self addToAddress.
					(tokens isEmpty not and: [#(Atom QuotedString ) includes: tokens last type])
						ifTrue: [self addToAddress]].
			self finishAddress]
]

{ #category : #parsing }
MailAddressParser >> grabGroupAddress [
	"grab an address of the form 'phrase : address, address, ..., address;'"
	"I'm not 100% sure what this format means, so I'm just returningthe list of addresses between the : and ;   -ls  (if this sounds right to someone, feel free to remove this comment :)"

	"remove the $; "
	tokens removeLast.

	"grab one address each time through this loop"
	[ 
		"remove commas"
		[
			tokens isEmpty not and: [ tokens last type = $, ]
		] whileTrue: [ tokens removeLast ].

		tokens isEmpty ifTrue: [
			"no matching :"
			^self error: 'stray ; in address list'. ].

		tokens last type = $:
	] whileFalse: [
		"delegate to either grabAddressWithRoute, or grabBasicAddress.  nested groups are not allowed"

		tokens last type = $> ifTrue: [
			self grabAddressWithRoute ]
		ifFalse: [ 
			(#(Atom DomainLiteral QuotedString) includes: tokens last type)  ifTrue: [
				self grabBasicAddress ]
		ifFalse: [
			^self error: 'un-recognized address format' ] ]
	].

	tokens removeLast.   "remove the :"

	self removePhrase.
]

{ #category : #parsing }
MailAddressParser >> grabName [

	| name |
	name := ''.
	[tokens notEmpty and: [#(Atom QuotedString $. $@) includes: (tokens last type) ]] 
		whileTrue: [ name := ' ' join: {tokens removeLast text copyWithoutAll: '"'. name} ].
	^ name trimBoth
]

{ #category : #initialization }
MailAddressParser >> initialize: tokenList [
	tokens := tokenList asOrderedCollection copy.
	addresses := OrderedCollection new.
	storeNames := false.
]

{ #category : #parsing }
MailAddressParser >> removePhrase [
	"skip most characters to the left of this"

	[
		tokens isNotEmpty and: [
			#(Atom QuotedString $. $@) includes: (tokens last type) ]
	] whileTrue: [ tokens removeLast ].

]

{ #category : #'building address list' }
MailAddressParser >> startNewAddress [
	"set up data structures to begin a new address"

	curAddrTokens notNil
		ifTrue: [ self error: 'starting new address before finishing the last one!' ].
	curAddrTokens := OrderedCollection new
]
