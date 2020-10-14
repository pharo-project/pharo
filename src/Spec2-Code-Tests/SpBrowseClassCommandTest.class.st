Class {
	#name : #SpBrowseClassCommandTest,
	#superclass : #SpToolCommandTest,
	#category : #'Spec2-Code-Tests'
}

{ #category : #accessing }
SpBrowseClassCommandTest >> commandClass [

	^ SpBrowseClassCommand
]

{ #category : #tests }
SpBrowseClassCommandTest >> testExecuteEvaluationError [
	| selectedTextOrLine object |

	command context systemNavigation
		when: #browse: do: [ :anObject | object := anObject ].
	command context
		when: #selectedTextOrLine do: [ selectedTextOrLine ].

	object := nil.
	selectedTextOrLine := '1000/0'.
	command execute.
	self assert: object equals: nil
]

{ #category : #tests }
SpBrowseClassCommandTest >> testExecuteEvaluationResult [
	| selectedTextOrLine object |

	command context systemNavigation
		when: #browse: do: [ :anObject | object := anObject ].
	command context
		when: #selectedTextOrLine do: [ selectedTextOrLine ].
		
	object := nil.
	selectedTextOrLine := '42'.
	command execute.
	self assert: object class equals: SmallInteger.
	
	object := nil.
	selectedTextOrLine := '1000 factorial'.
	command execute.
	self assert: object class equals: LargePositiveInteger
]

{ #category : #tests }
SpBrowseClassCommandTest >> testExecuteGlobal [
	| selectedTextOrLine object |

	command context systemNavigation
		when: #browse: do: [ :anObject | object := anObject ].
	command context
		when: #selectedTextOrLine do: [ selectedTextOrLine ].
		
	object := nil.
	selectedTextOrLine := 'Object'.
	command execute.
	self assert: object equals: Object
]
