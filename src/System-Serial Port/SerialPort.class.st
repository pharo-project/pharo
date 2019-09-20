"
This class supports a simple interface to the serial ports of the underlying platform, if it supports serial ports. The mapping of port numbers to hardware ports is platform specific, but typically follows platform ordering conventions. For example, on the Macintosh, port 0 is the modem port and port 1 is the printer port, since in the programmers documentation these ports are referred to as ports A and B.

"
Class {
	#name : #SerialPort,
	#superclass : #Object,
	#instVars : [
		'port',
		'baudRate',
		'stopBitsType',
		'parityType',
		'dataBits',
		'outputFlowControlType',
		'inputFlowControlType',
		'xOnByte',
		'xOffByte'
	],
	#category : #'System-Serial Port'
}

{ #category : #accessing }
SerialPort >> baudRate [

	^ baudRate

]

{ #category : #accessing }
SerialPort >> baudRate: anInteger [
	"Set the baud rate for this serial port."

	baudRate := anInteger
]

{ #category : #'open/close' }
SerialPort >> close [
	"Close the serial port. Do nothing if the port is not open."
	
	port ifNotNil: [port isString 
					ifTrue: [self primClosePortByName: port]
 					ifFalse: [self primClosePort: port]].
	port := nil
]

{ #category : #accessing }
SerialPort >> dataBits [

	^ dataBits

]

{ #category : #accessing }
SerialPort >> dataBits: anInteger [
	"Set the number of data bits for this serial port to 5, 6, 7, or 8."

	dataBits := anInteger

]

{ #category : #initialization }
SerialPort >> initialize [
	"Default port settings."

	super initialize.
	baudRate := 9600.	"9600 baud"
	stopBitsType := 1.	"one stop bit"
	parityType := 0.	"no parity"
	dataBits := 8.	"8 bits"
	outputFlowControlType := 0.	"none"
	inputFlowControlType := 0.	"none"
	xOnByte := 19.	"ctrl-S"
	xOffByte := 24	"ctrl-X"
]

{ #category : #accessing }
SerialPort >> inputFlowControlType [

	^ inputFlowControlType

]

{ #category : #accessing }
SerialPort >> inputFlowControlType: anInteger [
	"Set the type of input flow control, where:
		0 - none
		1 - XOn/XOff
		2 - hardware handshaking"

	inputFlowControlType := anInteger

]

{ #category : #'input/output' }
SerialPort >> nextPutAll: aStringOrByteArray [
	"Send the given bytes out this serial port. The port must be open."

	^ port isString
		ifTrue: [ self
				primWritePortByName: port
				from: aStringOrByteArray
				startingAt: 1
				count: aStringOrByteArray size ]
		ifFalse: [ self
				primWritePort: port
				from: aStringOrByteArray
				startingAt: 1
				count: aStringOrByteArray size ]
]

{ #category : #'open/close' }
SerialPort >> openPort: portId [

	"Open the given serial port, using the settings specified by my
	instance variables."

	| result |

	self close.	"8-10 - wks - this has to tell us if it fails: step 1, save the primitive result"
	result := portId isString
		ifTrue: [ self
				primOpenPortByName: portId
				baudRate: baudRate
				stopBitsType: stopBitsType
				parityType: parityType
				dataBits: dataBits
				inFlowControlType: inputFlowControlType
				outFlowControlType: outputFlowControlType
				xOnByte: xOnByte
				xOffByte: xOffByte
			]
		ifFalse: [ self
				primOpenPort: portId
				baudRate: baudRate
				stopBitsType: stopBitsType
				parityType: parityType
				dataBits: dataBits
				inFlowControlType: inputFlowControlType
				outFlowControlType: outputFlowControlType
				xOnByte: xOnByte
				xOffByte: xOffByte
			].	"wks - raise an error if there was a failure."
	result ifNil: [ self error: 'Cannot open ' , portId printString ].

	port := portId
]

{ #category : #accessing }
SerialPort >> outputFlowControlType [

	^ outputFlowControlType

]

{ #category : #accessing }
SerialPort >> outputFlowControlType: anInteger [
	"Set the type of output flow control, where:
		0 - none
		1 - XOn/XOff
		2 - hardware handshaking"

	outputFlowControlType := anInteger

]

{ #category : #accessing }
SerialPort >> parityType [

	^ parityType

]

{ #category : #accessing }
SerialPort >> parityType: anInteger [
	"Set the parity type for this serial port, where:
		0 - no parity
		1 - odd parity
		2 - even parity"

	parityType := anInteger

]

{ #category : #primitives }
SerialPort >> primClosePort: portNumber [

	<primitive: 'primitiveSerialPortClose' module: 'SerialPlugin'>
	^ nil  "(DNS)"
	"self primitiveFailed."

]

{ #category : #primitives }
SerialPort >> primClosePortByName: portName [
	<primitive: 'primitiveSerialPortCloseByName' module:'SerialPlugin'>
	^ nil"(DNS)"
	"self primitiveFailed."
]

{ #category : #primitives }
SerialPort >> primOpenPort: portNumber baudRate: baud stopBitsType: stop
	parityType: parity dataBits: numDataBits
	inFlowControlType: inFlowCtrl outFlowControlType: outFlowCtrl
	xOnByte: xOn xOffByte: xOff [

	<primitive: 'primitiveSerialPortOpen' module: 'SerialPlugin'>
	^ nil  "(DNS)"

]

{ #category : #primitives }
SerialPort >> primOpenPortByName: portName baudRate: baud stopBitsType: stop parityType: parity dataBits: numDataBits inFlowControlType: inFlowCtrl outFlowControlType: outFlowCtrl xOnByte: xOn xOffByte: xOff [ 
	<primitive: 'primitiveSerialPortOpenByName' module:'SerialPlugin'>
	^ nil"(DNS)"
]

{ #category : #primitives }
SerialPort >> primReadPort: portNumber into: byteArray startingAt: startIndex count: count [

	<primitive: 'primitiveSerialPortRead' module: 'SerialPlugin'>
	self primitiveFailed

]

{ #category : #primitives }
SerialPort >> primReadPortByName: portName into: byteArray startingAt: startIndex count: count [ 
	<primitive: 'primitiveSerialPortReadByName' module:'SerialPlugin'>
	self primitiveFailed
]

{ #category : #primitives }
SerialPort >> primWritePort: portNumber from: byteArray startingAt: startIndex count: count [

	<primitive: 'primitiveSerialPortWrite' module: 'SerialPlugin'>
	self primitiveFailed.

]

{ #category : #primitives }
SerialPort >> primWritePortByName: portName from: byteArray startingAt: startIndex count: count [ 
	<primitive: 'primitiveSerialPortWriteByName' module:'SerialPlugin'>
	self primitiveFailed
]

{ #category : #printing }
SerialPort >> printOn: aStream [

	aStream
		nextPutAll: 'SerialPort(';
		nextPutAll:
			(port ifNil: ['closed'] ifNotNil: ['#', port printString]);
		nextPutAll: ', ';
		print: baudRate; nextPutAll: ' baud, ';
		print: dataBits; nextPutAll: ' bits, ';
		nextPutAll: (#('1.5' '1' '2') at: stopBitsType + 1); nextPutAll: ' stopbits, ';
		nextPutAll: (#('no' 'odd' 'even') at: parityType + 1); nextPutAll: ' parity)'

]

{ #category : #'input/output' }
SerialPort >> readByteArray [
	"Answer a ByteArray read from this serial port. Answer an empty ByteArray if no data is available. The port must be open. "
	
	| buf count |
	buf := ByteArray new: 1000.
	count := port isString
				ifTrue: [self
						primReadPortByName: port
						into: buf
						startingAt: 1
						count: buf size]
				ifFalse: [self
						primReadPort: port
						into: buf
						startingAt: 1
						count: buf size].
	^ buf copyFrom: 1 to: count
]

{ #category : #'input/output' }
SerialPort >> readInto: aStringOrByteArray startingAt: index [
	"Read data into the given String or ByteArray object starting at the given index, and answer the number of bytes read. Does not go past the end of the given String or ByteArray."

	^ port isString
		ifTrue: [ self
				primReadPortByName: port
				into: aStringOrByteArray
				startingAt: index
				count: aStringOrByteArray size - index + 1 ]
		ifFalse: [ self
				primReadPort: port
				into: aStringOrByteArray
				startingAt: index
				count: aStringOrByteArray size - index + 1 ]
]

{ #category : #'input/output' }
SerialPort >> readString [
	"Answer a String read from this serial port. Answer the empty
	String if no data is available. The port must be open."
	| buf count |
	buf := String new: 1000.
	count := port isString 
				ifTrue: [ self
							primReadPortByName: port
							into: buf
							startingAt: 1
							count: buf size.]
				ifFalse: [ self
						 	primReadPort: port
							into: buf
							startingAt: 1
							count: buf size.].
	^ buf copyFrom: 1 to: count
]

{ #category : #accessing }
SerialPort >> stopBitsType [

	^ stopBitsType

]

{ #category : #accessing }
SerialPort >> stopBitsType: anInteger [
	"Set the stop bits type for this serial port, where:
		0 - 1.5 stop bits
		1 - one stop bit
		2 - two stop bits"

	stopBitsType := anInteger

]

{ #category : #accessing }
SerialPort >> xOffByte [

	^ xOffByte

]

{ #category : #accessing }
SerialPort >> xOffByte: anInteger [
	"Set the value of the XOff byte to be used if XOn/XOff flow control is enabled."

	xOffByte := anInteger

]

{ #category : #accessing }
SerialPort >> xOnByte [

	^ xOnByte

]

{ #category : #accessing }
SerialPort >> xOnByte: anInteger [
	"Set the value of the XOn byte to be used if XOn/XOff flow control is enabled."

	xOnByte := anInteger

]
