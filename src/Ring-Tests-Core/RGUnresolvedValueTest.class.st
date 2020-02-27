Class {
	#name : #RGUnresolvedValueTest,
	#superclass : #RGTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #tests }
RGUnresolvedValueTest >> testDefaultValue [

	| unresolved | 
	
	unresolved := RGUnresolvedValue new.
	self deny: unresolved isRingResolved.
	self deny: unresolved isRingFullyResolved.
	self assert: unresolved isRingFullyUnresolved.
	self assert: unresolved value isNil.
	self assert: unresolved value isRingResolved.
	
	unresolved := RGUnresolvedValue new default: #someObject.
	self deny: unresolved isRingResolved.
	self assert: unresolved value equals: #someObject.
	self assert: unresolved value isRingResolved.
	
	unresolved := RGUnresolvedValue new default: #someObject.
	self deny: unresolved isRingResolved.
	self assert: unresolved value equals: #someObject.
	self assert: unresolved value isRingResolved.

	unresolved := RGUnresolvedValue recursive.
	self deny: unresolved isRingResolved.
	self deny: unresolved value isRingResolved.

]

{ #category : #tests }
RGUnresolvedValueTest >> testReferencedBehaviors [

	| unresolved | 
	
	unresolved := RGUnresolvedValue new.
	self assert: unresolved referencedBehaviors isEmpty.
]
