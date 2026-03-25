Extension { #name : 'String' }

{ #category : '*Deprecated14' }
String >> asUrl [
	self deprecated: 'Use asZnUrl instead' transformWith: '`@rcv asUrl' -> '`@rcv asZnUrl'.
	^ self asZnUrl
]

{ #category : '*Deprecated14' }
String >> isValidSelector [
	"check I could be a valid selector (name of method).
	 For checking if there is symbol like me used as a selector, see #isSelectorSymbol on Symbol"

	self deprecated: 'Use OCScanner isSelector: ' 
		on: '02/02/2025'  
		in: 'pharo13' 
		transformWith: '`@receiver isValidSelector' -> 'OCScanner isSelector: `@receiver'.
	^ OCScanner isSelector: self
]

{ #category : '*Deprecated14' }
String >> metacelloIntegerLessThanSelf: anInteger [
	"integer version components are always '>' string component"

	^false
]

{ #category : '*Deprecated14' }
String >> metacelloSemanticIntegerLessThanSelf: anInteger [
    "integer version components are always '>' string component"

    ^ true
]

{ #category : '*Deprecated14' }
String >> metacelloSemanticStringLessThanSelf: aString [
    ^ aString < self
]

{ #category : '*Deprecated14' }
String >> metacelloSemanticVersionComponentLessThan: aMetacelloVersonComponent [
    ^ aMetacelloVersonComponent metacelloSemanticStringLessThanSelf: self
]

{ #category : '*Deprecated14' }
String >> metacelloStringLessThanSelf: aString [

	^aString < self
]

{ #category : '*Deprecated14' }
String >> metacelloVersionComponentLessThan: aMetacelloVersonComponent [

	^aMetacelloVersonComponent metacelloStringLessThanSelf: self
]

{ #category : '*Deprecated14' }
String >> parseLiterals [

	self 
		deprecated: 'Use OCParser parseLiterals: instead of String>>parseLiterals' 
		on: '29/12/2024' 
		in: 'pharo13' 
		transformWith: 
		'`@receiver parseLiterals' -> 'OCParser parseLiterals: `@receiver'.
	^ OCParser parseLiterals: self
]
