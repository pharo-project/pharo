"
Moving assignments outside blocks leads to shorter and more efficient code.
For example:
test 
	ifTrue: [var := 1]
	ifFalse: [var:= 2]
is equivalent to:
var :=  test 
	ifTrue: [1]
	ifFalse: [2]
"
Class {
	#name : 'ReAssignmentInIfTrueRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Optimization',
	#package : 'General-Rules',
	#tag : 'Optimization'
}

{ #category : 'accessing' }
ReAssignmentInIfTrueRule class >> group [
	^ 'Optimization'
]

{ #category : 'accessing' }
ReAssignmentInIfTrueRule class >> rationale [
	^ 'Moving assignments outside blocks leads to shorter and more efficient code.'
]

{ #category : 'accessing' }
ReAssignmentInIfTrueRule class >> ruleName [
	^ 'Move variable assignment outside of single statement ifTrue:ifFalse: blocks'
]

{ #category : 'accessing' }
ReAssignmentInIfTrueRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'AssignmentInIfTrueRule'
]

{ #category : 'initialization' }
ReAssignmentInIfTrueRule >> initialize [
	super initialize.
	self
		replace: '``@Boolean ifTrue: [`variable := ``@true] ifFalse: [`variable := ``@false]'
		with: '`variable := ``@Boolean ifTrue: [``@true] ifFalse: [``@false]';
		replace: '``@Boolean ifFalse: [`variable := ``@true] ifTrue: [`variable := ``@false]'
		with: '`variable := ``@Boolean ifFalse: [``@true] ifTrue: [``@false]'
]
