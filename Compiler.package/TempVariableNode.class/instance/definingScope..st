code generation (closures)
definingScope: scopeBlock 
	"<BlockNode>"
	definingScope ifNotNil: [self error: 'temp has more than one defining scope.  This is probably a parser error'].
	definingScope := scopeBlock