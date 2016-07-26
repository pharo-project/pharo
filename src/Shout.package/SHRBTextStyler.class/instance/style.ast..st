private
style: aText ast: ast
	text := aText.	
	charAttr := Array new: aText size withAll: (self attributesFor: #default).
	bracketLevel := 0.
	parentheseLevel:=0.
	ast acceptVisitor: self .
	aText runs: (RunArray newFrom: charAttr).	
