printing
printStringRadix: baseInteger 
	"Return a string containing a sequence of characters that represents the 
	numeric value of the receiver in the radix specified by the argument.  
	If the receiver is negative, a minus sign ('-') is prepended to the 
	sequence of characters. 
	The result is undefined if baseInteger less than 2 or greater than 36."
	
	| tempString |
	
	baseInteger = 10
		ifTrue: [
			tempString := self storeStringBase: baseInteger.
			self negative
				ifTrue: [^ '-10r' , (tempString copyFrom: 2 to: tempString size)]
				ifFalse: [^ '10r' , tempString]].
			
	^ self storeStringBase: baseInteger