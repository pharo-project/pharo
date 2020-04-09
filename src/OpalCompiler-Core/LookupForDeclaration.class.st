"
When looking up temp var declarations, we do not want the Requestor scope to automatically create that variable. OCRequestorScope is skipped if this variable is set.

See #lookupVarForDeclaration: how it is used.
"
Class {
	#name : #LookupForDeclaration,
	#superclass : #DynamicVariable,
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #accessing }
LookupForDeclaration >> default [
	^false
]
