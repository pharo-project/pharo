monticello menus behavior
commit: workingCopy in: aRepository
	| newVersion |

	newVersion := workingCopy newVersionIn: aRepository.
	newVersion ifNil: [ ^ self ].
	Cursor wait showWhile: [
		[ 	aRepository storeVersion: newVersion.
			aRepository storeDependencies: newVersion ] 
		ensure: [ (MCVersionInspector new version: newVersion) show ]]