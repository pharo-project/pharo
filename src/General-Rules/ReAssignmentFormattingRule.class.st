"
This rule verify if there is spaces when you make an assignement 

Prefer 
arg := 1 
over 
arg:=1 

"
Class {
	#name : 'ReAssignmentFormattingRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Formatting',
	#package : 'General-Rules',
	#tag : 'Formatting'
}

{ #category : 'accessing' }
ReAssignmentFormattingRule class >> group [ 

	^ 'Formatting'
]

{ #category : 'accessing' }
ReAssignmentFormattingRule class >> rationale [ 

	^ 'there should be an space between the temporary, ":=" and the assignment variable'
]

{ #category : 'accessing' }
ReAssignmentFormattingRule class >> ruleName [

	^ 'Assignment formatting'
]

{ #category : 'running' }
ReAssignmentFormattingRule >> basicCheck: aNode [ 

	| arg variable |

	aNode isAssignment ifFalse: [ ^ false ].
	arg := aNode children first.
	variable := aNode children second.
	^ variable stop + 5 > arg start 
]
