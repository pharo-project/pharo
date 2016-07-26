private
keyAtValue: value ifAbsent: exceptionBlock
	"Answer the key that is the external name for the argument, value. If 
	there is none, answer the result of evaluating exceptionBlock."
 
	self associationsDo: 
		[:association | value == association value ifTrue: [^ association key]].
	^ exceptionBlock value