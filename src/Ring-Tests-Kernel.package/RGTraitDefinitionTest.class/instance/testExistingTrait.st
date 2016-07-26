testing
testExistingTrait
	| newClass metaClass |
	
	newClass:= RGTraitDefinition named:  #TSortable.
	self assert: (newClass isTrait).
	self assert: (newClass isDefined).
	self assert: (newClass realClass = TSortable).
	self assert: (newClass isMeta not).
	
	newClass withMetaclass.
	self assert: (newClass hasMetaclass).
	metaClass:= newClass theMetaClass.
	self assert: (metaClass isMeta).
	self assert: (metaClass name = 'TSortable classTrait').
	self assert: (metaClass theNonMetaClass = newClass).
	self assert: (metaClass realClass = TSortable theMetaClass).