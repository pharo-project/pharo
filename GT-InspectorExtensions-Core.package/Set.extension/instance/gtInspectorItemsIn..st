*GT-InspectorExtensions-Core
gtInspectorItemsIn: composite
	"we override this because sets do not work well in morph tree morphs" 
	^ (super gtInspectorItemsIn: composite)
		display: [:set | set asOrderedCollection]