structure
root
	"Return the root of the composite morph containing the receiver. The owner of the root is either nil, a WorldMorph, or a HandMorph. If the receiver's owner is nil, the root is the receiver itself. This method always returns a morph."

	(owner isNil or: [owner isWorldOrHandMorph]) ifTrue: [^self].
	^owner root