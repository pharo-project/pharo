instance creation
from: srcString to: dstString inClass: srcClass
	"srcClass not used when not pretty printing, but have it for polymorphism with PrettyTextDiffBuilder"

	^self new from: srcString to: dstString