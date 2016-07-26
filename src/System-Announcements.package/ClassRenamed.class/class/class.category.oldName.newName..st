instance creation
class: aClass category: aCategoryName oldName: anOldClassName newName: aNewClassName
	^self new
			classRenamed: aClass;
			category: aCategoryName;
			oldName: anOldClassName;
			newName: aNewClassName;
			yourself