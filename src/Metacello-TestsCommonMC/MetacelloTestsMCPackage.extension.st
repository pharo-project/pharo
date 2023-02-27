Extension { #name : #MetacelloTestsMCPackage }

{ #category : #'*Metacello-TestsCommonMC' }
MetacelloTestsMCPackage >> packageInfo [
	"Aiming for speeding up the running of the tests"

	^ MetacelloTestsPackageSet named: name
]

{ #category : #'*Metacello-TestsCommonMC' }
MetacelloTestsMCPackage >> packageSet [
	"Aiming for speeding up the running of the tests"

	^ MetacelloTestsPackageSet named: name
]

{ #category : #'*Metacello-TestsCommonMC' }
MetacelloTestsMCPackage >> species [
	^ MCPackage
]
