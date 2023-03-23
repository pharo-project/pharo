"
I get signalled when a variable in a block or method scope shadows a variable of the same name in an outer scope.  The default action is to allow it.
"
Class {
	#name : #OCShadowVariableWarning,
	#superclass : #OCSemanticWarning,
	#category : #'OpalCompiler-Core-Exception'
}
