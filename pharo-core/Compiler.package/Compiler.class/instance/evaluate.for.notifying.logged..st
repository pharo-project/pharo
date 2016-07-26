public - old
evaluate: textOrString for: anObject notifying: aController logged: logFlag
	"Compile and execute the argument, textOrString with respect to the class 
	of anObject. If a compilation error occurs, notify aController. If both 
	compilation and execution are successful then, if logFlag is true, log 
	(write) the text onto a system changes file so that it can be replayed if 
	necessary."

	^ self		evaluate: textOrString
				in: nil
				to: anObject
				notifying: aController
				ifFail: [^nil]
				logged: logFlag.