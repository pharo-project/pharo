user interface
initialExtent
	
	 ^(self valueOfProperty: #initialExtent) ifNil: [ 700@500 ] ifNotNil: [ :ext | ext ]
