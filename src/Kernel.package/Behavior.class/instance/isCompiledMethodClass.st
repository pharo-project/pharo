testing
isCompiledMethodClass
	"Answer whether the receiver has compiled method instances that mix pointers and bytes."
	^self instSpec >= 24