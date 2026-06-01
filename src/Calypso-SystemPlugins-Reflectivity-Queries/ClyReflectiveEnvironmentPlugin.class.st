"
I am special plugin which decorate methods with various kind of metalinks: halts and breakpoint.
And I provide related method groups
- breakpoints
They group related methods together
"
Class {
	#name : 'ClyReflectiveEnvironmentPlugin',
	#superclass : 'ClySystemEnvironmentPlugin',
	#category : 'Calypso-SystemPlugins-Reflectivity-Queries-Plugin',
	#package : 'Calypso-SystemPlugins-Reflectivity-Queries',
	#tag : 'Plugin'
}

{ #category : 'groups collecting' }
ClyReflectiveEnvironmentPlugin >> collectMethodGroupProviders [

	^{ ClyBreakpointMethodGroupProvider new }
]

{ #category : 'item decoration' }
ClyReflectiveEnvironmentPlugin >> decorateBrowserItem: anItem ofMethod: aMethod [

	aMethod containsHalt ifTrue: [ anItem markWith: ClyMethodWithHaltTag ].
	aMethod hasBreakpoint ifTrue: [ anItem markWith: ClyMethodWithBreakpointTag ]
]

{ #category : 'item decoration' }
ClyReflectiveEnvironmentPlugin >> doesAnyManager: metalinkManagers belongsTo: aMethod [

	^ metalinkManagers anySatisfy: [ :each | 
		each isNotNil and: [ self doesNodeOf: each belongsTo: aMethod ] ]
]

{ #category : 'item decoration' }
ClyReflectiveEnvironmentPlugin >> doesNodeOf: aMetalinkManager belongsTo: aMethod [
	| methodNode |

	methodNode := aMetalinkManager node methodNode.
	methodNode selector == aMethod selector ifFalse: [ ^ false ].
	methodNode methodClass == aMethod methodClass ifFalse: [ ^ false ].
	methodNode == aMethod ast ifFalse: [ ^ false ].
	
	^ true
]
