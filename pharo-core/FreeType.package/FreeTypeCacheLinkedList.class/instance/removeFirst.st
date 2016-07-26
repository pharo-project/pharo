removing
removeFirst
	"Remove the first element and answer it. If the receiver is empty, create 
	an error notification."

	| oldLink |
	self emptyCheck.
	oldLink := firstLink.
	oldLink previousLink: nil.
	lastLink == firstLink 
		ifTrue:[
			lastLink := firstLink := nil.
			oldLink nextLink: nil.
			^oldLink].
	firstLink := oldLink nextLink.
	firstLink == nil 
		ifTrue:[firstLink := lastLink := nil]
		ifFalse:[firstLink previousLink: nil].
	oldLink nextLink: nil.
	^oldLink