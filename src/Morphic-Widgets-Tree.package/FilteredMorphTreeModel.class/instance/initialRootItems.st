accessing
initialRootItems
	"contains the full list, the filtered list is in rootItems"
	^ initialRootItems ifNil: [ initialRootItems := #() ]