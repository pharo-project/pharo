"
SUnit tests for byte symbols
"
Class {
	#name : #ByteSymbolTest,
	#superclass : #SymbolTest,
	#category : #'Collections-Tests-Strings'
}

{ #category : #'tests - creation' }
ByteSymbolTest >> testAs [
	
	| tStr tAs1 tAs2 |

	tStr := DateAndTime now asString .
	tAs1 :=  tStr as: ByteSymbol . 
	self assert: (Symbol allSymbols select: [:e | e asString  = tStr ]  ) size = 1 .
	self assert: (ByteSymbol allInstances select: [:e | e asString = tStr] ) size = 1 .  
	self assert: (ByteSymbol allInstances select: [:e | e asString = tStr] ) =
		(Symbol allSymbols select: [:e | e asString  = tStr ]  ) .
		
	tAs2 := tStr as: ByteSymbol . 
	self assert: (Symbol allSymbols select: [:e | e asString  = tStr ]  ) size = 1 .
	self assert: (ByteSymbol allInstances select: [:e | e asString = tStr] ) size = 1 .  


]

{ #category : #'tests - non-creation' }
ByteSymbolTest >> testNew [
	
	self should: [ByteSymbol new: 5 ] raise: self defaultTestError. 
]

{ #category : #'tests - creation' }
ByteSymbolTest >> testNewFrom [
	| dt newFrom1 newFrom2 |
	dt := DateAndTime now asString.
	newFrom1 := ByteSymbol newFrom: dt.
	self assert: (Symbol allSymbols select: [ :e | e asString = dt ]) size = 1.
	self assert: (ByteSymbol allInstances select: [ :e | e = dt ]) size = 1.
	self
		assert:
			(Symbol allSymbols select: [ :e | e asString = dt ])
				= (ByteSymbol allInstances select: [ :e | e = dt ]).
	newFrom2 := ByteSymbol newFrom: dt.
	self assert: (Symbol allSymbols select: [ :e | e asString = dt ]) size = 1.
	self assert: (ByteSymbol allInstances select: [ :e | e = dt ]) size = 1
]

{ #category : #'tests - creation' }
ByteSymbolTest >> testReadFromString [

	| str strWithPound readFrom1 readFrom2 |
	Smalltalk garbageCollect. 
	str := 'notYetExisting' .
	self assert: (Symbol allSymbols select: [:e | e asString = str ]) size = 0 .
	self assert: (ByteSymbol allInstances select: [:e | e asString = str]) size = 0 .  
	strWithPound := ('#' , str) .
	readFrom1 := ByteSymbol readFromString: strWithPound .
	self assert:	(Symbol allSymbols select: [:e | e asString = str ]) size = 1 .
	self assert:	(ByteSymbol allInstances select: [:e | e = str ]) size = 1 .
	self assert:	(Symbol allSymbols select: [:e | e asString  = str ]) =
			(ByteSymbol allInstances select: [:e | e = str ]) .
			
	readFrom2  := ByteSymbol readFromString: strWithPound .
	self assert:
		(Symbol allSymbols select: [:e | e asString  = str ]) size = 1.
	self assert:
		(ByteSymbol allInstances select: [:e | e = str]) size = 1.
]
