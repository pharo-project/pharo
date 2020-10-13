Class {
	#name : #SpBrowseClassHierarchyCommandTest,
	#superclass : #SpToolCommandTest,
	#category : #'Spec2-Code-Tests'
}

{ #category : #accessing }
SpBrowseClassHierarchyCommandTest >> commandClass [

	^ SpBrowseClassHierarchyCommand
]

{ #category : #tests }
SpBrowseClassHierarchyCommandTest >> testExecuteEvaluationError [
	| selectedTextOrLine object |

	command context systemNavigation
		when: #browseHierarchy: do: [ :anObject | object := anObject ].
	command context
		when: #selectedTextOrLine do: [ selectedTextOrLine ].

	object := nil.
	selectedTextOrLine := '1000/0'.
	command execute.
	self assert: object equals: nil
]

{ #category : #tests }
SpBrowseClassHierarchyCommandTest >> testExecuteEvaluationResult [
	| selectedTextOrLine class |

	command context systemNavigation
		when: #browseHierarchy: do: [ :anObject | class := anObject ].
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
SpBrowseClassHierarchyCommandTest >> testExecuteGlobal [
	| selectedTextOrLine object |

	command context systemNavigation
		when: #browseHierarchy: do: [ :anObject | object := anObject ].
	command context
		when: #selectedTextOrLine do: [ selectedTextOrLine ].
		
	object := nil.
	selectedTextOrLine := 'Object'.
	command execute.
	self assert: object equals: Object
]
