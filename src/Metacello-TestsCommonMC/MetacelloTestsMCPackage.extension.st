Extension { #name : #MetacelloTestsMCPackage }

{ #category : #'*metacello-testscommonmc' }
MetacelloTestsMCPackage >> packageInfo [
	"Aiming for speeding up the running of the tests"

	^ MetacelloTestsPackageSet named: name

]

{ #category : #'*metacello-testscommonmc' }
MetacelloTestsMCPackage >> packageSet [
	"Aiming for speeding up the running of the tests"

	^ MetacelloTestsPackageSet named: name

]

{ #category : #'*metacello-testscommonmc' }
MetacelloTestsMCPackage >> species [
	^ MCPackage

]
