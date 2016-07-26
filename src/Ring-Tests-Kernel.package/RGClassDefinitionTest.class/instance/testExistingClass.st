testing
testExistingClass
	| newClass metaClass |
	
	newClass:= RGClassDefinition named:  #OrderedCollection.
	self assert: (newClass isClass).
	self assert: (newClass isDefined).
	self assert: (newClass realClass = OrderedCollection).
	self assert: (newClass isMeta not).
	
	newClass withMetaclass.
	self assert: (newClass hasMetaclass).
	metaClass:= newClass theMetaClass.
	self assert: (metaClass isMeta).
	self assert: (metaClass name = 'OrderedCollection class').
	self assert: (metaClass theNonMetaClass = newClass).
	self assert: (metaClass realClass = OrderedCollection class).