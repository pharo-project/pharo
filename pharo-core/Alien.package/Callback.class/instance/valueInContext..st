evaluation
valueInContext: callbackContext "<VMCallbackContext32|VMCallbackContext64> ^<Integer>"
	"Perform the evaluator with arguments derived from the callback context in an order most appropriate for the ABI.
	 On x86 the order is callbackContext, stackPointerAlien.
	 On ``RISCS'' it is callbackContext, integerRegsAlien, stackPointerAlien, floatRegsAlien"
	self subclassResponsibility