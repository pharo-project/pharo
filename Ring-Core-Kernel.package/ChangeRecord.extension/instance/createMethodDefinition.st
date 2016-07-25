*Ring-Core-Kernel
createMethodDefinition
	"Retrieves a RGMethodDefinition object based on the data of the receiver"
	
	^(RGMethodDefinition named: self methodSelector)
		parentName: self methodClassName;
		isMetaSide: meta;  
		sourceCode: self string;
		protocol: category;
		stamp: stamp;
		yourself