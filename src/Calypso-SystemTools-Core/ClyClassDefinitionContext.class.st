Class {
	#name : 'ClyClassDefinitionContext',
	#superclass : 'ClySourceCodeContext',
	#category : 'Calypso-SystemTools-Core-Editors-Classes',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'Editors-Classes'
}

{ #category : 'testing' }
ClyClassDefinitionContext >> isArgOrTempVariableSelected [

	^ false
]

{ #category : 'testing' }
ClyClassDefinitionContext >> isAssignmentSelected [
	^ false
]

{ #category : 'testing' }
ClyClassDefinitionContext >> isClassDefinitionContext [

	^ true
]

{ #category : 'testing' }
ClyClassDefinitionContext >> isGlobalVariableSelected [
	^ false
]

{ #category : 'testing' }
ClyClassDefinitionContext >> isSelectedItemHasBreakpoint [
	^ false
]

{ #category : 'testing' }
ClyClassDefinitionContext >> isTempVariableSelected [
	^ false
]

{ #category : 'testing' }
ClyClassDefinitionContext >> isVariableSelected [
	^ false
]
