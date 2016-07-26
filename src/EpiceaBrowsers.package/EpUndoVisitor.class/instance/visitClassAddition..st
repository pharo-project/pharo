visitor
visitClassAddition: aClassCreated
	| classAffected |
	classAffected := aClassCreated realClassAffected.
	environment removeClassNamed: classAffected name