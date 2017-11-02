"
I keep track of symbols to be retrieved in ocompletion algorithm. 
Unlike ecompletion, I learn something and try to retrieve users last occurences of typing sequences first, then the system symbols which match. 

TODO remove stupid symbols (like packages and method extensions and timestamp)
"
Class {
	#name : #NOCCompletionTable,
	#superclass : #Object,
	#instVars : [
		'table',
		'numberofEntries'
	],
	#classInstVars : [
		'classTable',
		'table'
	],
	#category : 'NECompletion-OCompletion'
}

{ #category : #accessing }
NOCCompletionTable class >> classes [
	^ classTable ifNil: [classTable := self new entriesPerPrefix: 40]
]

{ #category : #cleanup }
NOCCompletionTable class >> cleanUp [
	self reset.
]

{ #category : #'instance creation' }
NOCCompletionTable class >> default [
	^ table ifNil: [table := self new entriesPerPrefix: 40]
]

{ #category : #'class initialization' }
NOCCompletionTable class >> initialize [
	
	self registerInterestToSystemAnnouncement.
			
	SessionManager default
		registerToolClassNamed: self name
]

{ #category : #events }
NOCCompletionTable class >> methodChanged: event [
	self default totalNumberOfEntries < 200 ifTrue: [self default quickFillWithCategoryOf: event methodClass].
	self default totalNumberOfEntries < 500 ifTrue: [self default quickFillWithClass: event methodClass].
	self default justCompiled: event selector in: event methodClass.
]

{ #category : #'class initialization' }
NOCCompletionTable class >> registerInterestToSystemAnnouncement [
	<systemEventRegistration>
	
	SystemAnnouncer uniqueInstance 
		unsubscribe: self.
		
	SystemAnnouncer uniqueInstance weak subscribe: MethodAdded, MethodModified 
			send: #methodChanged: 
			to: self.
		
]

{ #category : #'instance creation' }
NOCCompletionTable class >> reset [
	classTable := nil.
	table := nil.
]

{ #category : #'system startup' }
NOCCompletionTable class >> shutDown [
	self reset
]

{ #category : #protocol }
NOCCompletionTable >> addEntry: aString [ 
	| d |
	d := DateAndTime now.
	self 
		addEntry: aString
		date: d
]

{ #category : #protocol }
NOCCompletionTable >> addEntry: aString date: d [ 
	| lists |
	lists := self listsForPrefix: aString.
	lists do: [:e | e
		addEntry: aString
		date: d] 
]

{ #category : #protocol }
NOCCompletionTable >> compiled: selector in: class date: date [

	| method |
	self addEntry: selector date: date.
	self class classes addEntry: class theNonMetaClass name date: date.
	(class canUnderstand: selector) ifFalse: [ ^self ].
	method := class compiledMethodAt: selector ifAbsent: [ ^self ].
	method messages do: [ :m | self  addEntry: m date: date ].
	method literals do: [ :each |
		(each isVariableBinding and: [
			each key notNil and: [
				each key first isUppercase ] ])
					ifTrue: [ 
						self class classes addEntry: each key date: date ] ]
]

{ #category : #accessing }
NOCCompletionTable >> entriesMatching: prefix [
	| list |
	
	list := self listForPrefix: prefix.
	^ list entriesMatching: prefix
]

{ #category : #accessing }
NOCCompletionTable >> entriesPerPrefix: n [
	numberofEntries := n
]

{ #category : #accessing }
NOCCompletionTable >> fillRate [
	^ self totalNumberOfEntries / self maxNumberOfEntries 
]

{ #category : #initialization }
NOCCompletionTable >> initialize [
	table := Dictionary new.
]

{ #category : #protocol }
NOCCompletionTable >> justCompiled: selector in: class [ 
	| date |
	date := DateAndTime now.
	self 
		compiled: selector
		in: class
		date: date
]

{ #category : #accessing }
NOCCompletionTable >> listForPrefix: aString [ 
	| prefix |
	aString isEmpty ifTrue: [^ NOCEntryList ofSize: numberofEntries].
	prefix := aString size = 1 
			ifTrue: [ aString first asLowercase asString] 
			ifFalse: [(aString first: 2) collect: [:e | e asLowercase]].
			
	^ table 
		at: prefix
		ifAbsentPut: [ NOCEntryList ofSize: numberofEntries ]
]

{ #category : #accessing }
NOCCompletionTable >> listsForPrefix: aString [ 
	|  prefixes |
	aString isEmpty ifTrue: [^ Array with: (NOCEntryList ofSize: numberofEntries)].
	prefixes := OrderedCollection with: aString first asLowercase asString.
	 
	aString size > 1 
			ifTrue: [prefixes add: ((aString first: 2) collect: [:e | e asLowercase])].
	^ prefixes collect: [:prefix | table 
		at: prefix
		ifAbsentPut: [ NOCEntryList ofSize: numberofEntries ]]
]

{ #category : #accessing }
NOCCompletionTable >> maxNumberOfEntries [
	^ 26 * 26 * numberofEntries 
]

{ #category : #accessing }
NOCCompletionTable >> numberOfEntries [
	^ numberofEntries
]

{ #category : #protocol }
NOCCompletionTable >> quickFillWithCategoriesMatching: pattern [
	(Smalltalk organization categoriesMatching: pattern) do: [:category | 
		(Smalltalk organization classesInCategory: category) do: [:e | self quickFillWithClass: e]]
]

{ #category : #protocol }
NOCCompletionTable >> quickFillWithCategoryOf: cls [
	(Smalltalk organization classesInCategory: cls theNonMetaClass category) do: [:e | self quickFillWithClass: e]
]

{ #category : #protocol }
NOCCompletionTable >> quickFillWithClass: cls [
	cls methodsDo: [:e | self justCompiled: e selector in: cls]
	
]

{ #category : #accessing }
NOCCompletionTable >> reset [
	table := Dictionary new.
]

{ #category : #accessing }
NOCCompletionTable >> totalNumberOfEntries [
	^ table inject: 0 into: [:total :list | total + list numEntries]
]
