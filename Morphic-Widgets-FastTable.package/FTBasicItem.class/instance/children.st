accessing
children
	"I use a cache because my children can be call often."

	^ children ifNil: [ children := self calculateChildren ]