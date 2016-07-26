tests
testSuperclassChangeLayoutType
	"This case reproduces a MNU found loading Moose."

	| superclass class |
	superclass := self makeWithLayout: ByteLayout.
	class := self make: [ :builder |
		builder 
			superclass: superclass;
			layoutClass: ByteLayout;
			name: self anotherClassName.
		 ].
	"Change the layout of the superclass from bytes to pointer"
	self makeWithLayout: FixedLayout.

	superclass classLayout checkIntegrity.
	class classLayout checkIntegrity.
	
	self deny: superclass classLayout isBits.
	self assert: class classLayout isBits.