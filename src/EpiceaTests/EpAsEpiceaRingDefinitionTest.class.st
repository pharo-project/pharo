Class {
	#name : #EpAsEpiceaRingDefinitionTest,
	#superclass : #TestCase,
	#category : #'EpiceaTests-Model'
}

{ #category : #running }
EpAsEpiceaRingDefinitionTest >> definitions [
	"FIX: it should not have this external dependency to Fuel code entities."

	^ {	(FLSerializer >> #serialize:on:).
		(Object >> #fuelAccept:).
		FLSerializer.
		FLSerializer class. 
		FLTGlobalClassOrTraitSerializationTest.
		FLTGlobalClassOrTraitSerializationTest classTrait.
		} collect: #asEpiceaRingDefinition
]

{ #category : #tests }
EpAsEpiceaRingDefinitionTest >> testPackageIsString [

	self definitions do: [ :each |
		self assert: each package name isString.
		self assert: each package name isNotEmpty ]
]
