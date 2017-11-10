"
A trait for test purposes
"
Trait {
	#name : #TIdentityAddTest,
	#category : #Collections-Tests-Abstract
}

{ #category : #requirements }
TIdentityAddTest >> collection [
	^ self explicitRequirement
]

{ #category : #requirements }
TIdentityAddTest >> element [
	^ self explicitRequirement
]

{ #category : #'tests - identity adding' }
TIdentityAddTest >> equalNotIdenticalElement [
	^ self element copy
]

{ #category : #'tests - identity adding' }
TIdentityAddTest >> testIdentityAdd [
	| added oldSize |
	oldSize := self collection size.
	self collection add: self element.
	self deny: (self collection includes: self equalNotIdenticalElement).
	
	added := self collection add: self equalNotIdenticalElement.
	self assert: added == self equalNotIdenticalElement.
	self assert: (self collection includes: self equalNotIdenticalElement)
]
