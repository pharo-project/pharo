Trait {
	#name : #TMockWithUseAndSlot,
	#instVars : [
		'aSlot'
	],
	#traits : 'Trait3',
	#classTraits : 'Trait3 classTrait',
	#category : #'TraitsV2-Tests'
}

{ #category : #'as yet unclassified' }
TMockWithUseAndSlot >> accessingIV [

	^ aSlot
]
