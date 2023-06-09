Extension { #name : #RPackage }

{ #category : #'*SUnit-Tests' }
RPackage >> testSuite [

	| suite |

	suite := TestSuite named: self packageName.
	suite addTests: ((self classes select: #isTestCase) flatCollect: [:aTestClass | aTestClass suite tests]).

	^ suite
]
