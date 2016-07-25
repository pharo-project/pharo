*Ring-Core-Kernel
asActiveRingDefinition
	"Retrieves an active RGMethodDefinition object based on the data of the receiver.
	Source, protocol and stamp are retrieved from the compiled method"
	
	^ RGMethodDefinition new 
			name: self selector;
			parentName: self methodClass name;
			isMetaSide: self methodClass isMeta;
			asActive.
