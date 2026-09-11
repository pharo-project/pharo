Class {
	#name : 'SymbolTableTest',
	#superclass : 'TestCase',
	#category : 'Collections-Strings-Tests',
	#package : 'Collections-Strings-Tests'
}

{ #category : 'tests - enumeration' }
SymbolTableTest >> testAllSymbolsDoOnEmptyTable [

	| table count |
	table := SymbolTable new.

	count := 0.
	table allSymbolsDo: [ :e | count := count + 1 ].

	self assert: count equals: 0
]

{ #category : 'tests - enumeration' }
SymbolTableTest >> testAllSymbolsDoWithInternedSymbols [

	| table symbol found |
	table := SymbolTable new.
	symbol := table intern: 'toto'.

	found := Set new.
	table allSymbolsDo: [ :e | found add: e ].

	self assert: (found includes: symbol)
]

{ #category : 'tests - enumeration' }
SymbolTableTest >> testAllSymbolsDoWithInternedSymbolsAfterCompaction [

	| table symbol found |
	table := SymbolTable new.
	symbol := table intern: 'toto'.
	table compactSymbols.

	found := Set new.
	table allSymbolsDo: [ :e | found add: e ].

	self assert: (found includes: symbol)
]

{ #category : 'tests - enumeration' }
SymbolTableTest >> testAllSymbolsDoWithInternedSymbolsAfterRehashin [

	| table symbol found |
	table := SymbolTable new.
	symbol := table intern: 'toto'.
	table rehash.

	found := Set new.
	table allSymbolsDo: [ :e | found add: e ].

	self assert: (found includes: symbol)
]

{ #category : 'tests - enumeration' }
SymbolTableTest >> testAllSymbolsDoWithInternedSymbolsBeforeAndAfterCompaction [

	| table symbol found symbol2 |
	table := SymbolTable new.
	symbol := table intern: 'toto'.
	table compactSymbols.
	symbol2 := table intern: 'toto2'.

	found := Set new.
	table allSymbolsDo: [ :e | found add: e ].

	self assert: (found includes: symbol).
	self assert: (found includes: symbol2)
]

{ #category : 'tests - enumeration' }
SymbolTableTest >> testAllSymbolsDoWithInternedSymbolsBeforeAndAfterRehashing [
	| table symbol found symbol2 |
	table := SymbolTable new.
	symbol := table intern: 'toto'.
	table rehash.
	symbol2 := table intern: 'toto2'.

	found := Set new.
	table allSymbolsDo: [ :e | found add: e ].

	self assert: (found includes: symbol).
	self assert: (found includes: symbol2)
]

{ #category : 'tests - enumeration' }
SymbolTableTest >> testAllSymbolsOnEmptyTable [

	| table |
	table := SymbolTable new.
	self assert: table allSymbols isEmpty
]

{ #category : 'tests - enumeration' }
SymbolTableTest >> testAllSymbolsWithInternedSymbols [

	| table symbol symbols |
	table := SymbolTable new.
	symbol := table intern: 'toto'.

	symbols := table allSymbols.

	self assert: (symbols includes: symbol)
]

{ #category : 'tests - enumeration' }
SymbolTableTest >> testAllSymbolsWithInternedSymbolsAfterCompaction [

	| table symbol symbols |
	table := SymbolTable new.
	symbol := table intern: 'toto'.
	table compactSymbols.

	symbols := table allSymbols.

	self assert: (symbols includes: symbol)
]

{ #category : 'tests - enumeration' }
SymbolTableTest >> testAllSymbolsWithInternedSymbolsAfterRehashing [

	| table symbol symbols |
	table := SymbolTable new.
	symbol := table intern: 'toto'.
	table rehash.

	symbols := table allSymbols.

	self assert: (symbols includes: symbol)
]

{ #category : 'tests - enumeration' }
SymbolTableTest >> testAllSymbolsWithInternedSymbolsBeforeAndAfterCompaction [

	| table symbol symbols symbol2 |
	table := SymbolTable new.
	symbol := table intern: 'toto'.
	table compactSymbols.
	symbol2 := table intern: 'toto2'.

	symbols := table allSymbols.

	self assert: (symbols includes: symbol).
	self assert: (symbols includes: symbol2)
]

{ #category : 'tests - enumeration' }
SymbolTableTest >> testAllSymbolsWithInternedSymbolsBeforeAndAfterRehashing [

	| table symbol symbols symbol2 |
	table := SymbolTable new.
	symbol := table intern: 'toto'.
	table rehash.
	symbol2 := table intern: 'toto2'.

	symbols := table allSymbols.

	self assert: (symbols includes: symbol).
	self assert: (symbols includes: symbol2)
]

{ #category : 'tests - compaction' }
SymbolTableTest >> testCompactionDoesNotInternDuplicate [

	| table symbol1 symbol2 |
	table := SymbolTable new.

	symbol1 := table intern: 'toto'.
	table compactSymbols.
	symbol2 := table intern: 'toto'.
	
	self assert: symbol1 identicalTo: symbol2
]

{ #category : 'tests - compaction' }
SymbolTableTest >> testCompactionHasInternedOfExistingSymbol [

	| table symbol1 |
	table := SymbolTable new.
	symbol1 := table intern: 'toto'.

	table compactSymbols.

	self assert: (table hasInterned: 'toto')
]

{ #category : 'tests - compaction' }
SymbolTableTest >> testCompactionKeepsElement [

	| table symbol |
	table := SymbolTable new.
	symbol := table intern: 'toto'.
	table compactSymbols.

	self deny: table isEmpty
]

{ #category : 'tests - compaction' }
SymbolTableTest >> testCompactionKeepsInternedSymbolsFromDifferentTablesDifferent [

	| table1 table2 symbol1 symbol2 |
	table1 := SymbolTable new.
	symbol1 := table1 intern: 'toto'.

	table2 := SymbolTable new.
	symbol2 := table2 intern: 'toto'.
	
	table1 compactSymbols.
	
	self deny: symbol1 identicalTo: symbol2
]

{ #category : 'tests - compaction' }
SymbolTableTest >> testCompationFindsInternedOfExistingSymbol [

	| table symbol |
	table := SymbolTable new.
	symbol := table intern: 'toto'.
	
	table compactSymbols.

	self assert: (table findInterned: 'toto') identicalTo: symbol
]

{ #category : 'tests - interning' }
SymbolTableTest >> testDoesNotInternDuplicate [

	| table symbol1 symbol2 |
	table := SymbolTable new.

	symbol1 := table intern: 'toto'.
	symbol2 := table intern: 'toto'.
	
	self assert: symbol1 identicalTo: symbol2
]

{ #category : 'tests - interning' }
SymbolTableTest >> testDoesNotInternDuplicateSymbol [

	| table symbol1 symbol2 |
	table := SymbolTable new.

	symbol1 := table intern: 'toto'.
	symbol2 := table intern: symbol1.
	
	self assert: symbol1 identicalTo: symbol2
]

{ #category : 'tests - interning' }
SymbolTableTest >> testFindInternedOfExistingSymbol [

	| table1 symbol |
	table1 := SymbolTable new.
	symbol := table1 intern: 'toto'.

	self assert: (table1 findInterned: 'toto') identicalTo: symbol
]

{ #category : 'tests - interning' }
SymbolTableTest >> testFindInternedOfNonExistingSymbol [

	| table1 |
	table1 := SymbolTable new.

	self assert: (table1 findInterned: 'toto') isNil
]

{ #category : 'test - creation' }
SymbolTableTest >> testFromSymbols [

	| initialTable s1 s2 s3 symbols table |
	initialTable := SymbolTable new.
	s1 := initialTable intern: 'a'.
	s2 := initialTable intern: 'b'.
	s3 := initialTable intern: 'c'.
	symbols := initialTable allSymbols.

	table := SymbolTable fromSymbols: symbols.
	self assert: (table findInterned: 'a') notNil.
	self assert: (table findInterned: 'b') notNil.
	self assert: (table findInterned: 'c') notNil
]

{ #category : 'tests - interning' }
SymbolTableTest >> testHasInternedOfExistingSymbol [

	| table1 symbol1 |
	table1 := SymbolTable new.
	symbol1 := table1 intern: 'toto'.

	self assert: (table1 hasInterned: 'toto')
]

{ #category : 'tests - interning' }
SymbolTableTest >> testHasInternedOfNonExistingSymbol [

	| table1 |
	table1 := SymbolTable new.

	self deny: (table1 hasInterned: 'toto')
]

{ #category : 'tests - interning' }
SymbolTableTest >> testInternedSymbolsFromDifferentTablesAreDifferent [

	| table1 table2 symbol1 symbol2 |
	table1 := SymbolTable new.
	symbol1 := table1 intern: 'toto'.

	table2 := SymbolTable new.
	symbol2 := table2 intern: 'toto'.
	
	self deny: symbol1 identicalTo: symbol2
]

{ #category : 'tests - interning' }
SymbolTableTest >> testInternedSymbolsFromDifferentTablesAreDifferentInterningSymbol [

	| table1 table2 symbol1 symbol2 |
	table1 := SymbolTable new.
	symbol1 := table1 intern: 'toto'.

	table2 := SymbolTable new.
	symbol2 := table2 intern: symbol1.
	
	self deny: symbol1 identicalTo: symbol2
]

{ #category : 'tests - compaction' }
SymbolTableTest >> testRehashinFindsInternedOfExistingSymbol [

	| table symbol |
	table := SymbolTable new.
	symbol := table intern: 'toto'.
	
	table rehash.

	self assert: (table findInterned: 'toto') identicalTo: symbol
]

{ #category : 'tests - compaction' }
SymbolTableTest >> testRehashingDoesNotInternDuplicate [

	| table symbol1 symbol2 |
	table := SymbolTable new.

	symbol1 := table intern: 'toto'.
	table rehash.
	symbol2 := table intern: 'toto'.
	
	self assert: symbol1 identicalTo: symbol2
]

{ #category : 'tests - compaction' }
SymbolTableTest >> testRehashingHasInternedOfExistingSymbol [

	| table symbol1 |
	table := SymbolTable new.
	symbol1 := table intern: 'toto'.

	table rehash.

	self assert: (table hasInterned: 'toto')
]

{ #category : 'tests - compaction' }
SymbolTableTest >> testRehashingKeepsElement [

	| table symbol |
	table := SymbolTable new.
	symbol := table intern: 'toto'.
	table rehash.

	self deny: table isEmpty
]

{ #category : 'tests - compaction' }
SymbolTableTest >> testRehashingKeepsInternedSymbolsFromDifferentTablesDifferent [

	| table1 table2 symbol1 symbol2 |
	table1 := SymbolTable new.
	symbol1 := table1 intern: 'toto'.

	table2 := SymbolTable new.
	symbol2 := table2 intern: 'toto'.
	
	table1 rehash.
	
	self deny: symbol1 identicalTo: symbol2
]

{ #category : 'tests - interning' }
SymbolTableTest >> testSymbolInternsAddEntry [

	| table symbol |
	table := SymbolTable new.
	symbol := table intern: 'toto'.
	
	self deny: table isEmpty
]

{ #category : 'tests - interning' }
SymbolTableTest >> testSymbolTableStartsEmpty [

	| table |
	table := SymbolTable new.
	self assert: table isEmpty
]
