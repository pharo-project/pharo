accessing
keyAtIdentityValue: value ifAbsent: exceptionBlock
	"Answer the key that is the external name for the argument, value. If 
	there is none, answer the result of evaluating exceptionBlock.
	Note: There can be multiple keys with the same value. Only one is returned."
 
	self associationsDo: 
		[:association | value == association value ifTrue: [^association key]].
	^exceptionBlock value