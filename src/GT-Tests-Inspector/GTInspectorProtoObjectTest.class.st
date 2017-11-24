Class {
	#name : #GTInspectorProtoObjectTest,
	#superclass : #GLMMorphicTest,
	#category : #'GT-Tests-Inspector'
}

{ #category : #tests }
GTInspectorProtoObjectTest >> testInspectDoesNotCrash [
	window := GTInspector openOn: ProtoObject new 
]
