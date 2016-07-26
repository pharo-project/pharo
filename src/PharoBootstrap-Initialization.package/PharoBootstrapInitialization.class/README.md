I hold the code needed to initialize correctly a fresh generated bootstrap image.

To run the initialization, run:
	PharoBootstrapInitialization run
	
 ====== Potential problems:
1/	SmalltalkImage classVarNamed: 'StartUpList' => 
			MessageNotUnderstood: Association>>read
			Association(Object)>>doesNotUnderstand: #read
			SmalltalkImage class(Class)>>readClassVariableNamed:
			SmalltalkImage class(Class)>>classVarNamed:
		should be a LiteralVariable (slot) instead of an Association

2/	Trait method compilation does not propagate to users of the trait (ex: TBehavior>>#becomeCompact).

3/	Class class selectors => #()
		the method dictionary is empty.
		maybe the same pb as the following: Class traits

4/	TClass classTrait selectors => #()
		it looks like methods on class traits are not well compiled