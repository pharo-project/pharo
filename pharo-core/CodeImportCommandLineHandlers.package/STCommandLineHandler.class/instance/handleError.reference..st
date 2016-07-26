private
handleError: error reference: aReference
	"Print a header before failing on an errro / syntax notification from the the script loaded by the given request"
	| fileReference |
	
	"We ignore warnings for now"
	(error isKindOf: OCSemanticWarning)
		ifTrue: [ ^ error pass ].
		
	"try to get the proper file"
	fileReference := aReference.
	((error isKindOf: OCSemanticError) and: [ error requestor isNotNil ])
		ifTrue: [ fileReference := error requestor fileReference ].
	
	"spit out a warning if in headless mode, otherwise a debugger will popup"
	Smalltalk isHeadless 
		ifTrue: [ self inform: 'Errors in script loaded from ', fileReference fullName ].
															
	"for syntax errors we can use the default action"															
	(error isKindOf: SyntaxErrorNotification)
		ifTrue: [ ^ error defaultAction ].
	
	(error isKindOf: OCSemanticError)
		ifTrue: [ 
			self class printCompilerWarning: error.
			^ error resume: error defaultAction ].
	
	"otherwise resignal it"
	error pass.