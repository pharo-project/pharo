primitives
primOpenPort: portNumber baudRate: baud stopBitsType: stop
	parityType: parity dataBits: numDataBits
	inFlowControlType: inFlowCtrl outFlowControlType: outFlowCtrl
	xOnByte: xOn xOffByte: xOff

	<primitive: 'primitiveSerialPortOpen' module: 'SerialPlugin'>
	^ nil  "(DNS)"
