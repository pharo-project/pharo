Extension { #name : #RGContainerTest }

{ #category : #'*Ring-Deprecated-Tests-Monticello' }
RGContainerTest >> testRetrievingPackages [
	| rgClass rgMethod |
	
	rgClass := OrderedCollection asRingDefinition.
	rgClass package: (RGContainer packageOfClass: rgClass).
	self assert: rgClass package name = #'Collections-Sequenceable'.
	
	rgMethod := (RGMethodDefinition realClass: Class selector: #asRingDefinition).
	rgMethod package: (RGContainer packageOfMethod: rgMethod).
	self assert: rgMethod parent isNil.
	self assert: rgMethod package name = #'Ring-Deprecated-Core-Kernel'.

	rgMethod := (RGMethodDefinition realClass: OrderedCollection selector: #size).
	rgMethod package: (RGContainer packageOfMethod: rgMethod).
	self assert: rgMethod package isNil.

	rgMethod := (RGMethodDefinition class: rgClass selector: #size).
	rgMethod package: (RGContainer packageOfMethod: rgMethod).
	self assert: rgMethod parent notNil.
	self assert: rgMethod package notNil.
	self assert: rgMethod package = rgClass package
]
