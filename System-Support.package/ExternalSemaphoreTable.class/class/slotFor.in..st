private
slotFor: anObject in: aCollection
	"find the first empty slot, or nil if there is none"

	"The following was written in an atomic fashion using special methods with no suspension points, not sure if on purpose, but keeping it that way for now.
	Uses should be protected by the ProtectTable semaphore anyhow, but... it's too much work to reason 100% about it"

	| firstEmptyIndex |
	1 to: aCollection size do: [ :i | 
		| obj |
		obj := aCollection at: i.
		obj == anObject
			ifTrue: [ ^ i ].	"object already there, just return its index"
		(firstEmptyIndex isNil and: [ obj isNil ])
			ifTrue: [ firstEmptyIndex := i ] ].
	^ firstEmptyIndex