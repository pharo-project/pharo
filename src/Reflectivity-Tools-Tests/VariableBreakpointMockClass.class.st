"
I'm just a class with inst var and accesses to it
"
Class {
	#name : 'VariableBreakpointMockClass',
	#superclass : 'Object',
	#instVars : [
		'v1'
	],
	#classVars : [
		'ClassVar'
	],
	#category : 'Reflectivity-Tools-Tests',
	#package : 'Reflectivity-Tools-Tests'
}

{ #category : 'accessing' }
VariableBreakpointMockClass >> classVarRead [
	^ClassVar
]

{ #category : 'accessing' }
VariableBreakpointMockClass >> classVarWrite [
	^ClassVar := 1
]

{ #category : 'initialization' }
VariableBreakpointMockClass >> initialize [
	v1 := 0
]

{ #category : 'accessing' }
VariableBreakpointMockClass >> methodWithTempsAndArg: arg [
	| temp |
	temp := arg.
	^ temp
]

{ #category : 'accessing' }
VariableBreakpointMockClass >> v1 [ ^v1
]

{ #category : 'accessing' }
VariableBreakpointMockClass >> v1: anObject [
	v1 := anObject
]
