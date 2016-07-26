removing
removeLast
	"Remove the first element and answer it. If the receiver is empty, create 
	an error notification."

	| oldLink |
	self emptyCheck.
	oldLink := lastLink.
	oldLink nextLink: nil.
	lastLink == firstLink 
		ifTrue:[
			lastLink := firstLink := nil.
			oldLink previousLink: nil.
			^oldLink].
	lastLink := oldLink previousLink.
	lastLink == nil 
		ifTrue:[firstLink := lastLink := nil]
		ifFalse:[lastLink nextLink: nil].
	oldLink previousLink: nil.
	^oldLink