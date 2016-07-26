private
setNamedPrimitiveInformationFrom: fromMethod toMethod: toMethod
	"For named primitives, the first literal contains a special object that has information of the primitive. Example:
(StandardFileStream >> #primOpen:writable:) literalAt: 1 ----->>>>   #(#FilePlugin #primitiveFileOpen 0 147).
In this method we cope such information from one to another one."
	| spec |	
	spec := toMethod literalAt: 1.	
	spec replaceFrom: 1 to: spec size with: (fromMethod literalAt: 1) startingAt: 1.
	
