instance creation
variable: aVarName class: aClass classVariable: aBoolean 
	^(self variable: aVarName class: aClass)
		classVariable: aBoolean; yourself