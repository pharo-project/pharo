Extension { #name : #ClyExtendingPackagesQueryTest }

{ #category : #'*Calypso-SystemPlugins-Traits-Queries-Tests' }
ClyExtendingPackagesQueryTest >> testFromClassWhichTraitIsExtendedButNotItself [
	"I'm tagging this expected failure because I'm not sure now if it is appropriate to keep 
	 or not with the removal of the 'traits-as-multiple-inheritance' stuff."
	<expectedFailure>

	self queryFromScope: ClyClassScope of: ClyClassWithTraits.
	self assert: resultItems size equals: 0
]
