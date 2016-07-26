tests
testChangeLayoutTypeFromByte

	aClass := self makeWithLayout: ByteLayout.
	"Change the layout of the class from bytes to pointer"
	aClass := self 
		makeWithLayout: FixedLayout 
		andSlots: { #id. #name }.
	
	aClass classLayout checkIntegrity.
	
	self deny: aClass classLayout isBits.
	self assert: aClass instVarNames equals: { #id. #name }.