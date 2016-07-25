visiting
visitMessageNodeInCascade: aMessageNodeInCascade
	(theSelectBlock isNil or: [theSelectBlock value: aMessageNodeInCascade]) ifFalse:
		[^nil].
	theBlock value: aMessageNodeInCascade.
	^super visitMessageNodeInCascade: aMessageNodeInCascade