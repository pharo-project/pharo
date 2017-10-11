tests - operations on categories
testRenameCategoryAlsoRenameAllExtensionProtocols
	"test that when we rename a category, all corresponding extension protocols in the system are renamed"
	"self debug: #testRenameCategoryAlsoRenameAllExtensionProtocols"
	
	|XPackage YPackage ZPackage classInY classInZ| 
	self addXCategory.
	self addYCategory.
	self addZCategory.
	XPackage := self organizer packageNamed: #XXXXX.
	YPackage := self organizer packageNamed: #YYYYY.
	ZPackage := self organizer packageNamed: #ZZZZZ.
	
	classInY := self createNewClassNamed: 'ClassInYPackage' inCategory: 'YYYYY'.
	classInZ := self createNewClassNamed: 'ClassInZPackage' inCategory: 'ZZZZZ'. 
	
	self createMethodNamed:  'extensionFromXInClassInY'  inClass: classInY   inCategory: '*XXXXX'. 
	self createMethodNamed:  'longNameExtensionFromXInClassInY'  inClass: classInY   inCategory: '*XXXXX-subcategory'.     
	self createMethodNamed:  'extensionFromXInClassInZ'  inClass: classInZ   inCategory: '*XXXXX'.     
	    
	Smalltalk organization renameCategory: 'XXXXX' toBe: 'NewCategoryName'.
	
	self assert: (XPackage name = 'NewCategoryName').
	self assert: (classInY >> #extensionFromXInClassInY) category = '*NewCategoryName'.
	self assert: (classInY >> #longNameExtensionFromXInClassInY) category = '*NewCategoryName'.
	self assert: (classInZ >> #extensionFromXInClassInZ) category = '*NewCategoryName'.