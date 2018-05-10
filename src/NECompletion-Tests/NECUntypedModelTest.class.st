Class {
	#name : #NECUntypedModelTest,
	#superclass : #TestCase,
	#instVars : [
		'prefValueCase'
	],
	#category : #'NECompletion-Tests'
}

{ #category : #running }
NECUntypedModelTest >> setUp [
	super setUp.
	prefValueCase := NECPreferences caseSensitive.
	NECPreferences caseSensitive: true
]

{ #category : #running }
NECUntypedModelTest >> tearDown [

	NECPreferences caseSensitive: prefValueCase.
	super tearDown
]

{ #category : #tests }
NECUntypedModelTest >> testAdditionalLocals [
	| model locals |
	model := NECUntypedModel 
		class: NECTestClass
		temporaries: (OrderedCollection 
				with: (NECLocalEntry 
						contents: 'a'
						type: #local)
				with: (NECLocalEntry 
						contents: 'b'
						type: #local))
		additionals: (Array with: (NECLocalEntry 
				contents: 'veryImp'
				type: #local))
		variables: true
		selectors: true.
	model loadEntries.
	locals := model entriesOfType: #local.
	self assert: (locals includes: 'veryImp')
]

{ #category : #tests }
NECUntypedModelTest >> testCaseSensitive [
	| model locals |
	self assert: NECPreferences caseSensitive.
	model := NECUntypedModel 
		class: NECTestClass
		temporaries: OrderedCollection new.
	model loadEntries.
	locals := model entriesOfType: #instVar.
	self assert: locals size == 18.
	self assert: (locals includes: 'third').
	self assert: (locals includes: 'constantInteger').
	self assert: (locals includes: 'complexInit2').
	self assert: (locals includes: 'typeSuggestingParameter').
	model narrowWith: 'fo'
]

{ #category : #tests }
NECUntypedModelTest >> testCaseSensitivity [
	| model instances |
	self assert: NECPreferences caseSensitive.
	model := NECUntypedModel 
		class: NECTestClass
		temporaries: OrderedCollection new.
	model listLimit: Float infinity.
		
	model narrowWith: 'typesugg'.
	self assert: model isEmpty.
	
	NECPreferences caseSensitive: false.
	model narrowWith: 'typesugg'.
	instances := model entriesOfType: #instVar.
	self assert: instances size == 2.
	self assert: (instances includes: 'typeSuggestingParameter').
	
	model narrowWith: 'dict'.
	self assert: model notEmpty.
	self assert: ((model entriesOfType: #selector) includes: 'Dictionary')
]

{ #category : #tests }
NECUntypedModelTest >> testEmpty [
	| model |
	model := NECUntypedModel new.
	self assert: model isEmpty.
	self assert: model entries isEmpty.
	self assert: model entryCount == 0
]

{ #category : #tests }
NECUntypedModelTest >> testForClassInstVars [
	| model locals |
	model := NECUntypedModel 
		class: NECTestClass
		temporaries: OrderedCollection new.
	model loadEntries.
	locals := model entriesOfType: #instVar.
	self assert: locals size == 18.
	self assert: (locals includes: 'third').
	self assert: (locals includes: 'constantInteger').
	self assert: (locals includes: 'complexInit2').
	self assert: (locals includes: 'typeSuggestingParameter').
	locals := model entriesOfType: #self.
	self assert: (locals includes: 'self').
	locals := model entriesOfType: #super.
	self assert: (locals includes: 'super').
	model narrowWith: 'fo'.
	locals := model entriesOfType: #instVar.
	self assert: locals size == 1.
	self assert: (locals includes: 'fourth')
]

{ #category : #tests }
NECUntypedModelTest >> testForClassLocals [
	| model locals temps |
	temps := #('loc1' 'x2' 'bar' 'var' ) collect: 
		[ :each | 
		NECLocalEntry 
			contents: each
			type: #local ].
	model := NECUntypedModel 
		class: NECTestClass
		temporaries: temps.
	model loadEntries.
	locals := model entriesOfType: #local.
	self assert: locals size == 4.
	self assert: (locals includes: 'loc1').
	self assert: (locals includes: 'x2').
	self assert: (locals includes: 'bar').
	self assert: (locals includes: 'var').
	model narrowWith: 'l'.
	locals := model entriesOfType: #local.
	self assert: locals size == 1.
	self assert: (locals includes: 'loc1')
]

{ #category : #tests }
NECUntypedModelTest >> testForClassVars [
	| model classVars |
	model := NECUntypedModel 
		class: NECTestClass
		temporaries: OrderedCollection new.
	model loadEntries.
	classVars := model entriesOfType: #classVar.
	self assert: classVars size >= 2.
	self assert: (classVars includes: 'SuperClassVar').
	self assert: (classVars includes: 'ClassVar')
]

{ #category : #tests }
NECUntypedModelTest >> testMessage [
	| model |
	model := NECUntypedModel 
		class: NECTestClass
		temporaries: #('a' 'b' ).
	model loadEntries.
	self assert: model notEmpty.
	self assert: model hasMessage.
	self assert: model message = 'press key for selectors'.
	model narrowWith: 'b'.
	self shouldnt: model hasMessage.
	model narrowWith: ''.
	self assert: model hasMessage.
	self assert: model message = 'press key for selectors'.
	model narrowWith: 'hagadagadu'.
	self assert: model hasMessage.
	self assert: model message = 'no completions found'.
	model := NECUntypedModel new.
	self assert: model hasMessage.
	self assert: model message = 'press key for selectors'
]

{ #category : #tests }
NECUntypedModelTest >> testNarrowWith [
	| count model |
	model := NECUntypedModel new.
	model listLimit: Float infinity.
	
	self assert: model isEmpty.
	
	model narrowWith: 'b'.
	count := model entryCount.
	self assert: count == model entries size.
	self assert: model isEmpty not.
	
	model narrowWith: 'bar'.
	self assert: count > model entryCount.
	
	model narrowWith: 'barXXXX'.
	self assert: model isEmpty.
	
	model narrowWith: 'b'.
	self assert: count == model entryCount.
	
	model narrowWith: 'save'.
	self assert: model isEmpty not.
	
	model narrowWith: ''.
	self assert: model isEmpty
]

{ #category : #tests }
NECUntypedModelTest >> testNoEntriesWithSpace [
	| model separatorEntry |
	model := NECUntypedModel new.
	self assert: model isEmpty.
	model narrowWith: 'b'.
	separatorEntry := model entries detect: 
					[:each | 
					(each value asString detect: [:char | char isSeparator] ifNone: []) notNil]
				ifNone: [].
	self assert: separatorEntry isNil
]

{ #category : #tests }
NECUntypedModelTest >> testTitle [
	| model |
	model := NECUntypedModel
				class: NECTestClass
				temporaries: #().
	self assert: model title isNil
]
