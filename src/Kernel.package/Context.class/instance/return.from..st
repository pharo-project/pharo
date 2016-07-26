instruction decoding
return: value from: aSender 
	"For simulation.  Roll back self to aSender and return value from it.  Execute any unwind blocks on the way.  ASSUMES aSender is a sender of self"

	| newTop context |
	aSender isDead ifTrue: [
		^ self send: #cannotReturn: to: self with: {value} super: false ].
	newTop := aSender sender.
	context := self findNextUnwindContextUpTo: newTop.
	context ifNotNil: [
		^ self send: #aboutToReturn:through: to: self with: {value. context} super: false] .
	self releaseTo: newTop.
	newTop ifNotNil: [ newTop push: value ].
	^ newTop
