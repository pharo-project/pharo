"
An AbstractConditionCommand is command that creates `@condition metavariable. May be used only for condition statements

"
Class {
	#name : #AbstractConditionCommand,
	#superclass : #AbstractCommand,
	#category : #'RewriteTool-Command'
}

{ #category : #accessing }
AbstractConditionCommand >> abstractName [
	^ '``@condition'
]

{ #category : #execute }
AbstractConditionCommand >> execute [
	^ self abstractWithName: self abstractName andIndex: self panel conditionIndex
]
