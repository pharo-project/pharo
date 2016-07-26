meta-actions
potentialTargetsAt: aPoint 
	"Return the potential targets for the receiver.  
	This is derived from Morph>>potentialEmbeddingTargets."
	| realOwner |
	realOwner := self topRendererOrSelf
	owner
		ifNil: [^ #()].
	^ realOwner
		morphsAt: aPoint
		