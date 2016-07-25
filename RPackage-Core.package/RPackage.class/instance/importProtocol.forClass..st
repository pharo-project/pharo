private
importProtocol: aProtocol forClass: aClass 
	"import all the methods of a protocol as defined in the receiver." 
	
		(aClass organization listAtCategoryNamed: aProtocol)
			do: [:each | self addMethod: (aClass >> each)]
	
	