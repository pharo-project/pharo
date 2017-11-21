"
Specific context inspector for the bottom right of the debugger
"
Class {
	#name : #EyeDebuggerContextInspector,
	#superclass : #EyeMethodContextInspector,
	#category : #Spec-Debugger
}

{ #category : #list }
EyeDebuggerContextInspector >> addAllInstVars: elements [
	"Do nothing"
]

{ #category : #list }
EyeDebuggerContextInspector >> addInstancesVariable: elements [
	"Do nothing"
]

{ #category : #list }
EyeDebuggerContextInspector >> addSelf: elements [
	"Add self to the list of elements you can access in the inspector with the label thisContext"

	elements add: (ThisContextEyeElement host: self object).
]

{ #category : #list }
EyeDebuggerContextInspector >> addSpecialFields: elements [
	
	elements add: (DynamicEyeElement 
		host: self object 
		label: 'all temp vars' 
		value: [ String streamContents: [ :s | 
					self object tempNames withIndexDo: [ :name :index | 
						s 
							nextPutAll: name ;
							nextPut: Character space ;
							nextPut: $: ;
							nextPut: Character space ;
							nextPutAll: (self object namedTempAt: index) asString ;
							nextPut: Character lf ] ] ]).
]

{ #category : #list }
EyeDebuggerContextInspector >> addStackTop: elements [

	elements add: (DynamicEyeElement 
			host: self object 
			label: 'stackTop' 
			value: [ self object stackPtr > 0 ifTrue: [ self object top ] ifFalse: [ #emptyStack ] ] ).
]

{ #category : #list }
EyeDebuggerContextInspector >> generateElements [
	"generate the collection of elements to show in the inspector for the object inspected"

	| elements |
	elements := OrderedCollection new.
	self addSelf: elements.
	"only if object is not nil this elements are retrievable"
	self object
		ifNotNil: [ 
			self addStackTop: elements.
			self addAllInstVars: elements.
			self addSpecialFields: elements.
			self addInstancesVariable: elements.
			self addVariableFields: elements ].
	^ elements
]
