change logging
renameClass: class from: oldName to: newName 
	"Include indication that a class has been renamed."

	| recorder oldMetaClassName newMetaClassName |
	(recorder := self changeRecorderFor: oldName)
		noteChangeType: #rename;
		noteNewName: newName asSymbol.
		
	"store under new name (metaclass too)"
	changeRecords at: newName put: recorder.
	changeRecords removeKey: oldName.


	newMetaClassName := newName, ' class'.
	oldMetaClassName := oldName, ' class'.
	recorder := changeRecords at: oldMetaClassName ifAbsent: [^ nil].
	changeRecords at: newMetaClassName put: recorder.
	changeRecords removeKey: oldMetaClassName.
	recorder noteNewName: newMetaClassName