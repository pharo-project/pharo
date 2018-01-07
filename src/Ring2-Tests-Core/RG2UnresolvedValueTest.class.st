Class {
	#name : #RG2UnresolvedValueTest,
	#superclass : #RG2Test,
	#category : #'Ring2-Tests-Core'
}

{ #category : #tests }
RG2UnresolvedValueTest >> testDefaultValue [

	| unresolved | 
	
	unresolved := RG2UnresolvedValue new.
	self deny: unresolved isRing2Resolved.
	self deny: unresolved isRing2FullyResolved.
	self assert: unresolved isRing2FullyUnresolved.
	self assert: unresolved value isNil.
	self assert: unresolved value isRing2Resolved.
	
	unresolved := RG2UnresolvedValue new default: #someObject.
	self deny: unresolved isRing2Resolved.
	self assert: unresolved value equals: #someObject.
	self assert: unresolved value isRing2Resolved.
	
	unresolved := RG2UnresolvedValue new default: #someObject.
	self deny: unresolved isRing2Resolved.
	self assert: unresolved value equals: #someObject.
	self assert: unresolved value isRing2Resolved.

	unresolved := RG2UnresolvedValue recursive.
	self deny: unresolved isRing2Resolved.
	self deny: unresolved value isRing2Resolved.

]

{ #category : #tests }
RG2UnresolvedValueTest >> testReferencedBehaviors [

	| unresolved | 
	
	unresolved := RG2UnresolvedValue new.
	self assert: unresolved referencedBehaviors isEmpty.
]
