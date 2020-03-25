Extension { #name : #RGContainerTest }

{ #category : #'*Ring-Definitions-Tests-Monticello' }
RGContainerTest >> testRetrievingPackages [
	| rgClass rgMethod |
	rgClass := OrderedCollection asRingDefinition.
	rgClass package: (RGContainer packageOfClass: rgClass).
	self assert: rgClass package name equals: #'Collections-Sequenceable'.

	rgMethod := RGMethodDefinition realClass: Class selector: #asRingDefinition.
	rgMethod package: (RGContainer packageOfMethod: rgMethod).
	self assert: rgMethod parent isNil.
	self assert: rgMethod package name equals: #'Ring-Definitions-Core'.

	rgMethod := RGMethodDefinition realClass: OrderedCollection selector: #size.
	rgMethod package: (RGContainer packageOfMethod: rgMethod).
	self assert: rgMethod package isNil.

	rgMethod := RGMethodDefinition class: rgClass selector: #size.
	rgMethod package: (RGContainer packageOfMethod: rgMethod).
	self assert: rgMethod parent notNil.
	self assert: rgMethod package notNil.
	self assert: rgMethod package equals: rgClass package
]
