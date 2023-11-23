Extension { #name : 'Package' }

{ #category : '*SUnit-Tests' }
Package >> testSuite [

	| suite |

	suite := TestSuite named: self packageName.
	suite addTests: ((self classes select: #isTestCase) flatCollect: [:aTestClass | aTestClass suite tests]).

	^ suite
]
