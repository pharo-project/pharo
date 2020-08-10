"
SUnit tests for byte symbols
"
Class {
	#name : #ByteSymbolTest,
	#superclass : #SymbolTest,
	#category : #'Collections-Strings-Tests-Base'
}

{ #category : #'tests - creation' }
ByteSymbolTest >> testAs [
	| tStr |
	tStr := DateAndTime now asString.
	tStr as: ByteSymbol.
	self assert: (Symbol allSymbols select: [ :e | e asString = tStr ]) size equals: 1.
	self assert: (ByteSymbol allInstances select: [ :e | e asString = tStr ]) size equals: 1.
	self assert: (ByteSymbol allInstances select: [ :e | e asString = tStr ]) equals: (Symbol allSymbols select: [ :e | e asString = tStr ]).
	tStr as: ByteSymbol.
	self assert: (Symbol allSymbols select: [ :e | e asString = tStr ]) size equals: 1.
	self assert: (ByteSymbol allInstances select: [ :e | e asString = tStr ]) size equals: 1
]

{ #category : #'tests - non-creation' }
ByteSymbolTest >> testNew [
	
	self should: [ByteSymbol new: 5 ] raise: self defaultTestError. 
]

{ #category : #'tests - creation' }
ByteSymbolTest >> testNewFrom [
	| dt |
	dt := DateAndTime now asString.
	ByteSymbol newFrom: dt.
	self assert: (Symbol allSymbols select: [ :e | e asString = dt ]) size equals: 1.
	self assert: (ByteSymbol allInstances select: [ :e | e = dt ]) size equals: 1.
	self assert: (Symbol allSymbols select: [ :e | e asString = dt ]) equals: (ByteSymbol allInstances select: [ :e | e = dt ]).
	ByteSymbol newFrom: dt.
	self assert: (Symbol allSymbols select: [ :e | e asString = dt ]) size equals: 1.
	self assert: (ByteSymbol allInstances select: [ :e | e = dt ]) size equals: 1
]

{ #category : #'tests - creation' }
ByteSymbolTest >> testReadFromString [
	| str strWithPound |
	Smalltalk garbageCollect.
	str := 'notYetExisting'.
	self assert: (Symbol allSymbols select: [ :e | e asString = str ]) size equals: 0.
	self assert: (ByteSymbol allInstances select: [ :e | e asString = str ]) size equals: 0.
	strWithPound := '#' , str.
	ByteSymbol readFromString: strWithPound.
	self assert: (Symbol allSymbols select: [ :e | e asString = str ]) size equals: 1.
	self assert: (ByteSymbol allInstances select: [ :e | e = str ]) size equals: 1.
	self assert: (Symbol allSymbols select: [ :e | e asString = str ]) equals: (ByteSymbol allInstances select: [ :e | e = str ]).
	ByteSymbol readFromString: strWithPound.
	self assert: (Symbol allSymbols select: [ :e | e asString = str ]) size equals: 1.
	self assert: (ByteSymbol allInstances select: [ :e | e = str ]) size equals: 1
]
