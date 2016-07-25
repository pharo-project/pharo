testing
testConvertingMCTraitDefinition
	| mcClass ringClass |
	
	mcClass := TSortable asClassDefinition.
	ringClass := mcClass asRingDefinition.
	
	self assert: (ringClass isTrait).
	self assert: (ringClass theMetaClass realClass = TSortable theMetaClass).
	self assert: (ringClass isSameRevisionAs: TSortable asRingDefinition).
	self deny: (ringClass isSameRevisionAs: TBehavior asRingDefinition)