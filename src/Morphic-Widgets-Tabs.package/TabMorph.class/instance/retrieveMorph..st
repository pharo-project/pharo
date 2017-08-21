protocol
retrieveMorph: aBlock
	"This is a hard request. The result is blocking contrary to softRetrieveMorph.
	Late binding so the morph may be built only on demand"

	morph ifNotNil: [ 
		aBlock value: morph.
		^ nil ].
	retrievingSemaphore isSignaled
		ifFalse: [ self softRetrieveMorph ].

	^ [ retrievingSemaphore wait.
		morph ifNotNil: [ 
			UIManager default defer: [aBlock value: morph ]]] 
				fork