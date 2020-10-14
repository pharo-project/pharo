Class {
	#name : #SpBrowseClassReferencesCommandTest,
	#superclass : #SpToolCommandTest,
	#category : #'Spec2-Code-Tests'
}

{ #category : #accessing }
SpBrowseClassReferencesCommandTest >> commandClass [

	^ SpBrowseClassReferencesCommand
]

{ #category : #tests }
SpBrowseClassReferencesCommandTest >> testExecuteEvaluationError [
	| selectedTextOrLine object |

	command context systemNavigation
		when: #browseAllCallsOnClass: do: [ :anObject | object := anObject ].
	command context
		when: #selectedTextOrLine do: [ selectedTextOrLine ].

	object := nil.
	selectedTextOrLine := '1000/0'.
	command execute.
	self assert: object equals: nil
]

{ #category : #tests }
SpBrowseClassReferencesCommandTest >> testExecuteEvaluationResult [
	| selectedTextOrLine class |

	command context systemNavigation
		when: #browseAllCallsOnClass: do: [ :anObject | class := anObject ].
	command context
		when: #selectedTextOrLine do: [ selectedTextOrLine ].
		
	class := nil.
	selectedTextOrLine := '42'.
	command execute.
	self assert: class equals: SmallInteger.
	
	class := nil.
	selectedTextOrLine := '1000 factorial'.
	command execute.
	self assert: class equals: LargePositiveInteger
]

{ #category : #tests }
SpBrowseClassReferencesCommandTest >> testExecuteGlobal [
	| selectedTextOrLine object |

	command context systemNavigation
		when: #browseAllCallsOnClass: do: [ :anObject | object := anObject ].
	command context
		when: #selectedTextOrLine do: [ selectedTextOrLine ].
		
	object := nil.
	selectedTextOrLine := 'Object'.
	command execute.
	self assert: object equals: Object
]
