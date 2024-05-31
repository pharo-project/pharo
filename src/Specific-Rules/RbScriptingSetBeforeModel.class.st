"
You have to call scripting methods after #model: or it will not work
"
Class {
	#name : 'RbScriptingSetBeforeModel',
	#superclass : 'ReInvocationSequenceRule',
	#category : 'Specific-Rules-Rubric',
	#package : 'Specific-Rules',
	#tag : 'Rubric'
}

{ #category : 'accessing' }
RbScriptingSetBeforeModel class >> group [
	^ 'Rubric'
]

{ #category : 'initialization' }
RbScriptingSetBeforeModel >> initialize [
	super initialize.

	self add: #(beForSmalltalkScripting beForSmalltalkCode) requiresPreSend: #model:
]
