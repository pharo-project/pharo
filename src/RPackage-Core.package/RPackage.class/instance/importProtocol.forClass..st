private
importProtocol: aProtocol forClass: aClass 
	"import all the methods of a protocol as defined in the receiver." 
	
		(aClass organization listAtCategoryNamed: aProtocol)
			do: [:each | 
				| aCompiledMethod |
				aCompiledMethod := (aClass >> each).
				aCompiledMethod isFromTrait ifFalse: [ self addMethod: aCompiledMethod ]].
	
	