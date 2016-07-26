initialization
initializeShortcuts
	self bindKeyCombination: $[ meta toAction: [ self inspectPrevious ].
	self bindKeyCombination: $] meta toAction: [ self inspectNext ].

	self bindKeyCombination: $[ meta shift toAction: [ self usePreviousInspectorType ].
	self bindKeyCombination: $] meta shift toAction: [ self useNextInspectorType ].

	self bindKeyCombination: $0 meta toAction: [ self useBasicInspector ].
	self bindKeyCombination: $0 meta shift toAction: [ self useDefaultInspector ].
	
	"code duplication? yes, but still easier than capturing arguments ;)"
	self bindKeyCombination: $1 meta toAction: [ self useInspectorAt: 1 ].
	self bindKeyCombination: $2 meta toAction: [ self useInspectorAt: 2 ].
	self bindKeyCombination: $3 meta toAction: [ self useInspectorAt: 3 ].
	self bindKeyCombination: $4 meta toAction: [ self useInspectorAt: 4 ].
	self bindKeyCombination: $5 meta toAction: [ self useInspectorAt: 5 ].
	self bindKeyCombination: $6 meta toAction: [ self useInspectorAt: 6 ].
	self bindKeyCombination: $7 meta toAction: [ self useInspectorAt: 7 ].
	self bindKeyCombination: $8 meta toAction: [ self useInspectorAt: 8 ].
	self bindKeyCombination: $9 meta toAction: [ self useInspectorAt: 9 ].