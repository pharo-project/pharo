private
handleError: error	
	
	"for syntax errors we can used the default action"	"otherwise resignal it"
	(error isKindOf: SyntaxErrorNotification)
		ifTrue: [ error defaultAction ]
		ifFalse: [ error pass ]