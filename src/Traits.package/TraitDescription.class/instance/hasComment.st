accessing comment
hasComment
	"return whether this class truly has a comment other than the default"
	| org |
	org := self instanceSide organization.
	^org classComment isEmptyOrNil not