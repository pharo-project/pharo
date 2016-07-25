*Ring-Core-Kernel
asHistoricalRingDefinition

	"Retrieves a historical RGMethodDefinition object based on the data of the receiver.
	Source, protocol and stamp are retrieved from the source file method"
	| ring |
	ring := (RGMethodDefinition named: self selector)
				parentName: self methodClass name;
				isMetaSide: self methodClass isMeta.

	self sourcePointer isZero
		ifTrue: [ "this should not happen but sometimes the system looks corrupted"
			ring protocol: self category;
				sourceCode: self sourceCode;
				stamp: self timeStamp ]
		ifFalse: [ 
			ring sourcePointer: self sourcePointer ].
	ring asHistorical.	
	
	^ ring