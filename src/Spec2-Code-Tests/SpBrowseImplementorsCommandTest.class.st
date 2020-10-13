Class {
	#name : #SpBrowseImplementorsCommandTest,
	#superclass : #SpToolCommandTest,
	#category : #'Spec2-Code-Tests'
}

{ #category : #accessing }
SpBrowseImplementorsCommandTest >> commandClass [

	^ SpBrowseImplementorsCommand
]

{ #category : #tests }
SpBrowseImplementorsCommandTest >> testExecuteKeywordSelector [
	<expectedFailure>
	| selectedTextOrLine selectorRef |

	command context 
		when: #selectedTextOrLine do: [ selectedTextOrLine ].
	command context systemNavigation 
		when: #browseAllImplementorsOf: do: [ :aSelector | selectorRef := aSelector ].

	selectorRef := nil.
	selectedTextOrLine := 'addItemLabeled: ''Some string'' do:'.
	command execute.
	self assert: selectorRef equals: #addItemLabeled:do:.
]

{ #category : #tests }
SpBrowseImplementorsCommandTest >> testExecuteReceiverPlusKeywordSelector [
	| selectedTextOrLine selectorRef |

	command context 
		when: #selectedTextOrLine do: [ selectedTextOrLine ].
	command context systemNavigation 
		when: #browseAllImplementorsOf: do: [ :aSelector | selectorRef := aSelector ].

	selectorRef := nil.
	selectedTextOrLine := 'self initialize: 42'.
	command execute.
	self assert: selectorRef equals: #initialize:.

	selectorRef := nil.
	selectedTextOrLine := 'self initialize: 42 and: other'.
	command execute.
	self assert: selectorRef equals: #initialize:and:.

]

{ #category : #tests }
SpBrowseImplementorsCommandTest >> testExecuteReceiverPlusUnarySelector [
	| selectedTextOrLine selectorRef |

	command context 
		when: #selectedTextOrLine do: [ selectedTextOrLine ].
	command context systemNavigation 
		when: #browseAllImplementorsOf: do: [ :aSelector | selectorRef := aSelector ].

	selectorRef := nil.
	selectedTextOrLine := 'self initialize'.
	command execute.
	self assert: selectorRef equals: #initialize.
]

{ #category : #tests }
SpBrowseImplementorsCommandTest >> testExecuteUnarySelector [
	| selectedTextOrLine selectorRef |

	command context 
		when: #selectedTextOrLine do: [ selectedTextOrLine ].
	command context systemNavigation 
		when: #browseAllImplementorsOf: do: [ :aSelector | selectorRef := aSelector ].

	selectorRef := nil.
	selectedTextOrLine := 'initialize'.
	command execute.
	self assert: selectorRef equals: #initialize
]
