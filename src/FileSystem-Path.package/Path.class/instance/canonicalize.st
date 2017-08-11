navigating
canonicalize
	"Answer the receiver with references to the current folder (.) and parent folder (..) removed"
	
	^self class withAll: (self class canonicalizeElements: self segments).