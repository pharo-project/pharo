initialization
initialize

	super initialize.
	initializedClasses := Set new.
	compactClasses := EPDictionary new.
	definitionMirrorMap := EPDictionary new.
	mirrorDefinitionMap := EPDictionary new.
	
	"maybe we could use a state pattern but for now this is so simple"
	useClassBuilder := false.
	
	fullyLoaded := Dictionary new