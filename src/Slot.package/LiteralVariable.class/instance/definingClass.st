queries
definingClass
	"The class defining the variable. For Globals, return nil"
	^Smalltalk globals allClasses detect: [ :class | class hasClassVariable: self ] ifNone: [ nil ]