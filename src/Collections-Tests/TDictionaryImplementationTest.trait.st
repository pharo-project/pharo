"
A trait for test purposes
"
Trait {
	#name : #TDictionaryImplementationTest,
	#category : #'Collections-Tests-Unordered'
}

{ #category : #'tests - implementation' }
TDictionaryImplementationTest >> testAtNil [
	"(self run: #testAtNil)"

	"nil is a valid key in Pharo. In VW nil is not a valid key"

	"Ansi 1.9 p, 168
    		5.7.2.5 Message: at: key put: newElement
    		Synopsis
    			Store newElement at key in the receiver. Answer newElement.
    		Definition: <abstractDictionary>
    		If lookup succeeds for key, then newElement replaces the element previously stored at key.
    		Otherwise, the newElement is stored at the new key. In either case, subsequent successful
    		lookups for key will answer newElement.  Answer newElement.

    		The result is undefined if the key is nil.

		This clearly indicates that different smalltalks where doing different assumptions."

	| dict1 |
	dict1 := self emptyDict.
	dict1 at: nil put: #none.
	self assert: (dict1 at: nil) = #none
]

{ #category : #'tests - implementation' }
TDictionaryImplementationTest >> testFindElementOrNil [
	"Set>>findElementOrNil: takes an associaiton (when called on a dictionary) an integer, the index of the first
	position at is either equal to the assocation or which is nil"
	| assoc indexForG |
	assoc := #g -> 100.
	self assert: (self nonEmptyDict findElementOrNil: assoc key) = (self nonEmptyDict array indexOf: nil).
	
	indexForG := (#g hash \\ self emptyDict array size) + 1.
	self assert: (self emptyDict findElementOrNil: assoc key) = ((self emptyDict array indexOf: nil) max: indexForG).
]

{ #category : #'tests - implementation' }
TDictionaryImplementationTest >> testNew [
	| d |
	d := self classToBeTested new: 10.
	self assert: d size = 0.
	
	"Why 14? Mysterious"
	self assert: d capacity = 14
]

{ #category : #'tests - implementation' }
TDictionaryImplementationTest >> testPseudo [
	"(self run: #testPseudo)"

	"true and false are valid keys"

	| dict1 |
	dict1 := self emptyDict.
	dict1 at: true put: #true.
	self assert: (dict1 at: true) = #true.
	dict1 at: false put: #false.
	self assert: (dict1 at: false) = #false
]

{ #category : #'tests - implementation' }
TDictionaryImplementationTest >> testPseudoVariablesAreValidKeys [
	"(self run: #testPseudoVariablesAreValidKeys)"

	"true and false are valid keys"

	| dict1 |
	dict1 := self emptyDict.
	dict1 at: true put: #true.
	self assert: (dict1 at: true) = #true.
	dict1 at: false put: #false.
	self assert: (dict1 at: false) = #false
]

{ #category : #'tests - implementation' }
TDictionaryImplementationTest >> testScanFor [
	"Set>>scanFor: return an integer "
	| assoc indexForG |
	assoc := #g -> 100.
	self assert: (self nonEmptyDict scanFor: assoc) = (self nonEmptyDict array indexOf: nil).

	indexForG := (#g hash \\ self emptyDict array size) + 1.
	self assert: (self emptyDict scanFor: assoc) = ((self emptyDict array indexOf: nil) max: indexForG).

]

{ #category : #'tests - implementation' }
TDictionaryImplementationTest >> testSpecies [
	self assert: self nonEmptyDict species == self nonEmptyDict class.
	self assert: self emptyDict species == self emptyDict class
]
