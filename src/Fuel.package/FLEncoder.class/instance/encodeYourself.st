encoding
encodeYourself

	self encodePositiveInteger: objectCount.
	self encodePositiveInteger: FLPlatform current isBigEndian asBit.