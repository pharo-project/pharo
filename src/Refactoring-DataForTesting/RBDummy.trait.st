Trait {
	#name : #RBDummy,
	#instVars : [
		'var1'
	],
	#category : #'Refactoring-DataForTesting'
}

{ #category : #'tests - some' }
RBDummy >> methodFromTrait [
	RBClassToRename justForTest
]

{ #category : #accessing }
RBDummy >> var1 [
	^ var1
]
