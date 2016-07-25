converting
asString 
	"Refer to the comment in String|asString."
	| newString |
	newString := self species new: self size.
	newString replaceFrom: 1 to: newString size with: self startingAt: 1.
	^newString