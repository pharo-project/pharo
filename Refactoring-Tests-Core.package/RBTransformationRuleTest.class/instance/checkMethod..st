accessing
checkMethod: aSmalllintContext 
	class := aSmalllintContext selectedClass.
	(rewriteRule executeTree: aSmalllintContext parseTree) ifTrue: 
			[(RecursiveSelfRule executeTree: rewriteRule tree initialAnswer: false)
				ifFalse: 
					[builder compile: rewriteRule tree printString
						in: class
						classified: aSmalllintContext protocols]]