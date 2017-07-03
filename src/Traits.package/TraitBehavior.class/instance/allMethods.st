accessing method dictionary
allMethods
	"Return the collection of compiled method I and my superclasses are defining"
	"asArray is used to not bump into a bug when comparing compiled methods."
	
	^ self allSelectors asArray collect: [ :s | self lookupSelector: s ]