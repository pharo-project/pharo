testing
definesClass: aClass
	"Returns true if the class, aClass, is one of the locally defined classes"
	"should be probably removed since this is the same as includesClass: or the inverse"
	^ self includesClass: aClass