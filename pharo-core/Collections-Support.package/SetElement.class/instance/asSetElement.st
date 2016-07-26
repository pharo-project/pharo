converting
asSetElement
	"A receiver has to be included into a set, as a distinct object.
	We need to wrap receiver in own turn, otherwise #enclosedSetElement will return wrong object for that set"
	^ SetElement with: self