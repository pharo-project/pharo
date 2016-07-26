accessing
comment
	"Return the first comment of the receiver"
	"(self>>#comment) comment"
	
	^ self methodClass firstPrecodeCommentFor: self selector