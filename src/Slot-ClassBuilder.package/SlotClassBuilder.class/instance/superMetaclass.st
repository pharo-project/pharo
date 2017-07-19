accessing
superMetaclass
	 ^ superclass ifNil: [ Class ] ifNotNil: [ superclass class ].