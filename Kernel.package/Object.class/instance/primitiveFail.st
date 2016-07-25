primitive failure
primitiveFail
	"primitiveFail may be invoked by certain methods whose code is translated in C. In such a case primitiveFail and not primitiveFailed
	 should be invoked. The reason is that this code is translated to C by VMMaker. #primitiveFail is 
	implemented in Interpreter of VMMaker."

	^ self primitiveFailed