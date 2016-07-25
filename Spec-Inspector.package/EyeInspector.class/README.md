To do a specific inspector subclass this and override 	EyeInspector>>addSpecialFields
Then on your object override
	Object>>inspectorClass
so it returns your new inspector