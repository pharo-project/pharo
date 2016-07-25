set implementation
enclosedSetElement
	"The receiver is included into a set as an element. 
	Since some objects require wrappers (see SetElement) to be able to be included into a Set,
	a set sends this message to its element to make sure it getting real object,
	instead of its wrapper.
	Only SetElement instance or its subclasses allowed to answer something different than receiver itself"
	
