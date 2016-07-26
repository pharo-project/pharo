testing
testCoreSystem
	#(#Object #Behavior #Boolean #True #False #Integer #SmallInteger #Collection #String)
		do: [ :each | 
			| class |
			class := Smalltalk globals classNamed: each.
			self
				formatClass: class;
				formatClass: class class ]