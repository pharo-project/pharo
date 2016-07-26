tests
testChangeLayoutTypeToByte
	aClass := self 
		makeWithLayout: FixedLayout
		andSlots: { #id. #name }.
	"Change the layout of the class from pointer to bytes"
	aClass := self makeWithLayout: ByteLayout.
	
	aClass classLayout checkIntegrity.
	
	self assert: aClass classLayout isBits.
	self assert: aClass instVarNames isEmpty.