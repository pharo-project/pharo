private
nameFor: varName
	| name |
	
	name:= varName copy.
	name at: 1 put: name first asLowercase.
	^name.