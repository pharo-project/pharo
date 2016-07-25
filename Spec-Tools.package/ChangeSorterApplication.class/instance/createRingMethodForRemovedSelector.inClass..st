private
createRingMethodForRemovedSelector: aSelector inClass: aClass
	"Create RingMethod from the	removealinfo"

	| removalInfo method |
	method := RGMethodDefinition class: aClass selector: aSelector.
	removalInfo := (self selectedChangeSet changeRecorderFor: aClass) methodChanges at: aSelector ifAbsent: [ ^ method ].
	method sourcePointer: removalInfo methodInfoFromRemoval first.
	^ method