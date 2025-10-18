Extension { #name : 'Symbol' }

{ #category : '*Deprecated12' }
Symbol class >> compactSymbolTable [
	"this used to create a set with just 25% empty space, but that reduces lookup performance"
		self
		deprecated: 'Use #rehash instead'
		transformWith: '`@receiver compactSymbolTable' -> '`@receiver rehash'.
	self rehash
]

{ #category : '*Deprecated12' }
Symbol class >> findInternedSelector: aString [
	"return a selector if the argument is used as a selector, nil if not"
	self deprecated: 'use #isSelectorSymbol instead'.
	^ (self findInterned: aString) ifNotNil: [ :symbol |
		  (self selectorTable like: symbol) ifNotNil: [ :selectorSymbol |
			  selectorSymbol ] ]
]

{ #category : '*Deprecated12' }
Symbol class >> lookup: aStringOrSymbol [
	self
		deprecated: 'Use #findInterned: instead'
		transformWith: '`@receiver lookup: `@statements1' -> '`@receiver findInterned: `@statements1'.

	^ self findInterned: aStringOrSymbol
]
