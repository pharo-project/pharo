code generation (closures)
actualScope
	"Answer the actual scope for the receiver.  If this is an unoptimized block then it is its
	 actual scope, but if this is an optimized block then the actual scope is some outer block."
	^actualScopeIfOptimized ifNil: [self]