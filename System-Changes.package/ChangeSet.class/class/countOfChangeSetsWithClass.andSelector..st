services
countOfChangeSetsWithClass: aClass andSelector: aSelector
	"Answer how many change sets record a change for the given class and selector"

	^ (self allChangeSetsWithClass: aClass selector: aSelector) size