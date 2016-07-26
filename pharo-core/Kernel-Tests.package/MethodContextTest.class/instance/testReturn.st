tests
testReturn
	"Why am I overriding setUp? Because sender must be thisContext, i.e, testReturn, not setUp."
	aMethodContext := Context sender: thisContext receiver: aReceiver method: aCompiledMethod arguments: #(). 
	self assert: (aMethodContext return: 5) = 5.