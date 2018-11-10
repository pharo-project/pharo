Extension { #name : #ClyExtendingPackagesTests }

{ #category : #'*Calypso-SystemPlugins-Traits-Queries-Tests' }
ClyExtendingPackagesTests >> testFromClassWhichTraitIsExtendedButNotItself [

	self queryFromScope: ClyClassScope of: ClyClassWithTraits.

	self assert: resultItems size equals: 0
]
