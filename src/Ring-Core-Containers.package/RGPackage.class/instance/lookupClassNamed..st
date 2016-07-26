lookup
lookupClassNamed: className
	"Look for the class named className in the receiver's defined classes or in any of its package children"
	| class |
	
	class:= self classNamed: className.
	class ifNotNil:[ ^class ].
	
	self packages
		do:[ :each| class:= each lookupClassNamed: className.
				    class ifNotNil:[ ^class ] ].
	^nil