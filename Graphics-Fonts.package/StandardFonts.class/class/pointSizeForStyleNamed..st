styles
pointSizeForStyleNamed: aSymbol
	"
	self pointSizeForStyleNamed: #small.
	self pointSizeForStyleNamed: #Medium.
	self pointSizeForStyleNamed: #LARGE.
	self pointSizeForStyleNamed: #verylarge.
	self pointSizeForStyleNamed: #Huge.
	self pointSizeForStyleNamed: #xxxxx.
	"
	^ (#(#(#small 9) #(#medium 12) #(#large 14) #(veryLarge 18) #(huge 24)) 
		detect: [:info | info first asUppercase = aSymbol asUppercase] ifNone: []) 
			ifNotNil: [:info | info second]
	
		
