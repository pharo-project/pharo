removing
remove: aLinkOrObject ifAbsent: aBlock 
	"Remove aLink from the receiver. If it is not there, answer the result of evaluating aBlock."
	
	| link |
	link := self linkOf: aLinkOrObject ifAbsent: [^aBlock value].
	self removeLink: link ifAbsent: [^aBlock value].
	^aLinkOrObject