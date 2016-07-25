evaluating
valueWithReceiver: aReceiver arguments: anArray 
	"This should be changed when all the VM will support passign of extra arguments
		^self receiver: aReceiver withArguments: anArray executeMethod: self"
	
	^ aReceiver withArgs: anArray executeMethod: self