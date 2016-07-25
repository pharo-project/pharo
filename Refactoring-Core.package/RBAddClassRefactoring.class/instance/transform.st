transforming
transform
	(self model)
		defineClass: ('<1p> subclass: #<2s> instanceVariableNames: '''' classVariableNames: '''' poolDictionaries: '''' category: <3p>' 
					expandMacrosWith: superclass
					with: className
					with: category asString);
		reparentClasses: subclasses to: (self model classNamed: className asSymbol)