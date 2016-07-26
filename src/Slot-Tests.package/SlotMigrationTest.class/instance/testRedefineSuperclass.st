tests
testRedefineSuperclass
	"This case reproduces a MNU found loading Moose."

	| superclass class |
	superclass := self makeWithLayout: FixedLayout.
	class := self make: [ :builder |
		builder 
			superclass: superclass;
			layoutClass: ByteLayout;
			name: self anotherClassName.
		 ].

	self makeWithLayout: FixedLayout.

	superclass classLayout checkIntegrity.
	class classLayout checkIntegrity.