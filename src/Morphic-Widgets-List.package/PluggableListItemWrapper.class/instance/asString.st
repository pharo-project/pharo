accessing
asString
	string ifNotNil:[^string].
	getStringSelector ifNil:[^super asString].
	^self sendToModel: getStringSelector
