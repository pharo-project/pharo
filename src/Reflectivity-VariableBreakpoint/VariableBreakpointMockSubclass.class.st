"
I have instvar accesses and I have a super class with variables that I access
"
Class {
	#name : #VariableBreakpointMockSubclass,
	#superclass : #VariableBreakpointMockClass,
	#instVars : [
		'v2'
	],
	#category : #'Reflectivity-VariableBreakpoint'
}

{ #category : #initialize }
VariableBreakpointMockSubclass >> initialize [
	super initialize.
	v2 := 0
]

{ #category : #accessing }
VariableBreakpointMockSubclass >> instVarReads [
	^v1 + v2
]

{ #category : #accessing }
VariableBreakpointMockSubclass >> instVarWrites [
	v1 := 4.
	v2 := 2
]

{ #category : #accessing }
VariableBreakpointMockSubclass >> v2 [
	^ v2
]

{ #category : #accessing }
VariableBreakpointMockSubclass >> v2: anObject [
	v2 := anObject
]
