"
A trait mock using another trait and a simple slot slot to be used in tests
"
Trait {
	#name : #TMockWithUseAndSlot,
	#instVars : [
		'simpleSlot'
	],
	#traits : 'Trait3',
	#classTraits : 'Trait3 classTrait',
	#category : #'TraitsV2-Tests'
}

{ #category : #accessing }
TMockWithUseAndSlot >> simpleSlot [

	^ simpleSlot
]
