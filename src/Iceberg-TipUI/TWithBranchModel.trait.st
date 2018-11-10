Trait {
	#name : #TWithBranchModel,
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #branches }
TWithBranchModel >> branchModels [
	^ self branches collect: [ :each | (IceTipBranchModel repositoryModel: self repositoryModel on: each) beCached ]
]

{ #category : #branches }
TWithBranchModel >> branches [
	^ self explicitRequirement
]

{ #category : #accessing }
TWithBranchModel >> defaultBranchSelection [
	^ self branchModels
		detect: #isHead
		ifNone: [ self hasBranches
				ifTrue: [ self branchModels ifNotEmpty: #anyOne ] ]
]

{ #category : #testing }
TWithBranchModel >> hasBranches [ 
	^ self branches isNotEmpty
]

{ #category : #accessing }
TWithBranchModel >> repositoryModel [ 
	^ self explicitRequirement
]
