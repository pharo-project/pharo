updating
updateDefinedClassNamed: oldString withNewName: newString
	"this method should only be used with class names , and not metaclass names"

	(oldString substrings size > 1 or: [ newString substrings size > 1 ])
		ifTrue: [ Error signal: 'You should not use metaclass names' ].	"=> update the 'classDefinedSelectors' dictionary (replace the old key by the new one)
       => update the 'metaclassDefinedSelectors' dictionary (replace the old key by the new one)"
	classes remove: oldString.
	classes add: newString.	"Don't forget that class tags also register the classes names."
	self classTags do: [ :aTag | aTag updateDefinedClassNamed: oldString withNewName: newString ].
	self flag: #cyrille.	"what happens if this is a metaclass: it breaks the invariant of the package"	"worse so far the second block below breaks"	"the name of the method should be updateDefinedClassNamed: withNewName:"	"CD: i only use this method when a class is renamed in the system (systemClassRenamedActionFrom:). Normally we are not able to rename a metaclass in the system (?) so we should never end up in a case with metaclass names ?  "
	(classDefinedSelectors at: oldString ifAbsent: [ nil ])
		ifNotNil: [ 
			classDefinedSelectors at: newString put: (classDefinedSelectors at: oldString).
			classDefinedSelectors
				removeKey: oldString
				ifAbsent: [ self reportBogusBehaviorOf: #updateDefinedClassNamed:withNewName: ] ].
	(metaclassDefinedSelectors at: oldString ifAbsent: [ nil ])
		ifNotNil: [ 
			metaclassDefinedSelectors at: newString put: (metaclassDefinedSelectors at: oldString).
			metaclassDefinedSelectors
				removeKey: oldString
				ifAbsent: [ self reportBogusBehaviorOf: #updateDefinedClassNamed:withNewName: ] ]