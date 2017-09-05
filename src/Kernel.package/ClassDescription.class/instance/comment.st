accessing comment
comment
	"Answer the receiver's comment. (If missing, supply a template) "
	| aString |
	aString := self instanceSide organization classComment.
	aString isEmpty ifFalse: [^ aString].
	^self classCommentBlank