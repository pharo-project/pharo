image reading/writing
understandsImageFormat: aStream
	(PluginBasedJPEGReadWriter understandsImageFormat: aStream) ifTrue:[^false].
	aStream reset.
	aStream next = 16rFF ifFalse: [^ false].
	aStream next = 16rD8 ifFalse: [^ false].
	^true