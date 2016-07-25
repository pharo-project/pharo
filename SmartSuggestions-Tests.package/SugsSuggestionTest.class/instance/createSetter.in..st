private
createSetter: varName in: aClass 
	| target name |
	
	target := self targetFor: varName in: aClass.
	name := self nameFor: varName.	
	target
		compile: ('<1s> anObject<n><t><2s> := anObject' expandMacrosWith: (name,':') with: varName)
		classified: #accessing.
	
	