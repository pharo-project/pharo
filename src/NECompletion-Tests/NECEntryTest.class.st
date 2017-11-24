Class {
	#name : #NECEntryTest,
	#superclass : #TestCase,
	#category : #'NECompletion-Tests'
}

{ #category : #tests }
NECEntryTest >> testCompletion [
	| entry |
	entry := NECSelectorEntry 
		contents: 'makeItHappen'
		type: #unary:.
	self assert: entry completion = #makeItHappen
]

{ #category : #tests }
NECEntryTest >> testInstance [
	| entry |
	entry := NECInstVarEntry contents: 'abc'.
	self assert: entry isInstance
]

{ #category : #tests }
NECEntryTest >> testLocal [
	| entry |
	entry := NECLocalEntry 
		contents: 'abc'
		type: #patternArg:.
	self assert: entry contents = 'abc'.
	self assert: entry type = #patternArg:.
	self assert: entry isLocal
]

{ #category : #tests }
NECEntryTest >> testPrintOn [
	| entry |
	entry := NECSelectorEntry 
		contents: 'compute'
		type: #unary:.
	self assert: 'NECSelectorEntry(compute,unary:)' = entry printString
]
