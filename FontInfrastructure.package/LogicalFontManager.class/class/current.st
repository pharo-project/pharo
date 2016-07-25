accessing
current
	"
	current := nil.
	self current
	"
	^current ifNil:[current := self defaultCurrent]