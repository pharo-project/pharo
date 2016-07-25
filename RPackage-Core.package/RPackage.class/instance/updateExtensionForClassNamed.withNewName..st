updating
updateExtensionForClassNamed: oldString withNewName: newString

	"=> update the 'classExtensionsSelectors' dictionary (replace the old key by the new one)
        => update the 'metaclassclassExtensionsSelectors' dictionary (replace the old key by the new one)"
	"classes remove: oldString.
	classes add: newString."
	
	(metaclassExtensionSelectors at: oldString ifAbsent: [nil]) ifNotNil: [
		metaclassExtensionSelectors at: newString put: (metaclassExtensionSelectors at: oldString).
		metaclassExtensionSelectors removeKey: oldString  ifAbsent: [self reportBogusBehaviorOf: #updateExtensionForClassNamed:withNewName:   ]
		].
	
	(classExtensionSelectors at: oldString ifAbsent: [nil]) ifNotNil: [
		classExtensionSelectors at: newString put: (classExtensionSelectors at: oldString).
		classExtensionSelectors removeKey: oldString ifAbsent: [self reportBogusBehaviorOf: #updateExtensionForClassNamed:withNewName:   ]
		].