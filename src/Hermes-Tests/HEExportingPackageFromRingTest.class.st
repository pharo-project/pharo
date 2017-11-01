Class {
	#name : #HEExportingPackageFromRingTest,
	#superclass : #HEExportingPackageTest,
	#category : 'Hermes-Tests'
}

{ #category : #'as yet unclassified' }
HEExportingPackageFromRingTest >> fillOldPackage: aPackageNamed [
	aPackage := self rfiPackageFor: aPackageNamed.
	old := HERingToHermesBuilder new visitPackage: aPackage.

]

{ #category : #'as yet unclassified' }
HEExportingPackageFromRingTest >> rfiPackageFor: aPackageNamed [
	| environment rfienvironment |
	environment := Dictionary new.
	(((DADependencyChecker new daPackageFor: aPackageNamed) dependentPackages collect: #packageName)
		, {aPackageNamed asSymbol. #Traits. #Kernel. #'Collections-Native'. #'Collections-Abstract'} collect: [ :e | RPackageOrganizer default packageNamed: e ])
		do: [ :pck | 
			| version |
			version := FiMonticelloWriter new written: pck mcWorkingCopy snapshot name: pck name.
			environment at: pck name ifAbsentPut: version ].
	rfienvironment := environment values asFiSet asRFiEnvironment.
	^ rfienvironment packages detect: [ :p | p name = aPackageNamed asSymbol ]
]

{ #category : #tests }
HEExportingPackageFromRingTest >> testExportingCollectionsTests [
	self skip.
]
