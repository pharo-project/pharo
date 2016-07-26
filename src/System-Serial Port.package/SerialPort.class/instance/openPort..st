open/close
openPort: portId 
	"Open the given serial port, using the settings specified by my
	instance variables."

		| result |	

	self close.
	
	"8-10 - wks - this has to tell us if it fails: step 1, save the primitive result"
	result := portId isString
		ifTrue: [self
				primOpenPortByName: portId
				baudRate: baudRate
				stopBitsType: stopBitsType
				parityType: parityType
				dataBits: dataBits
				inFlowControlType: inputFlowControlType
				outFlowControlType: outputFlowControlType
				xOnByte: xOnByte
				xOffByte: xOffByte]
		ifFalse: [self
				primOpenPort: portId
				baudRate: baudRate
				stopBitsType: stopBitsType
				parityType: parityType
				dataBits: dataBits
				inFlowControlType: inputFlowControlType
				outFlowControlType: outputFlowControlType
				xOnByte: xOnByte
				xOffByte: xOffByte].

	"wks - raise an error if there was a failure."
	result isNil ifTrue:[ self error:'Cannot open ', portId printString. ].

	port := portId
