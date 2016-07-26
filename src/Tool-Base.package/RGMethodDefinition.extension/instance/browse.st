*Tool-Base
browse

	^ Smalltalk tools browser
		openOnClass: self realClass 
		selector: (self isDefined ifTrue: [ self selector ] ifFalse: [ nil ])