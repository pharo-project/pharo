private
createGetter: varName in: aClass
	| target name |

	target := self targetFor: varName in: aClass.
	name := self nameFor: varName.	
	target
		compile: ('<1s><n><t>^ <2s>' expandMacrosWith: name with: varName)
		classified: #accessing.
	
	