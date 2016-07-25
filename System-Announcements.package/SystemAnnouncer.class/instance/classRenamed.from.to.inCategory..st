triggering
classRenamed: aClass from: oldClassName to: newClassName inCategory: aCategoryName 
	self announce: (ClassRenamed 
				class: aClass
				category: aCategoryName
				oldName: oldClassName
				newName: newClassName)