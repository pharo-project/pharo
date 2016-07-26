accessing
hasContents
	hasContentsSelector ifNil:[^super hasContents].
	^self sendToModel: hasContentsSelector
