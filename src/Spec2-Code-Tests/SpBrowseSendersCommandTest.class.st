Class {
	#name : #SpBrowseSendersCommandTest,
	#superclass : #SpToolCommandTest,
	#category : #'Spec2-Code-Tests'
}

{ #category : #accessing }
SpBrowseSendersCommandTest >> commandClass [

	^ SpBrowseSendersCommand
]

{ #category : #tests }
SpBrowseSendersCommandTest >> testExecuteGlobal [
	| selectedTextOrLine varRef globalRef selectorRef |

	command context systemNavigation
		when: #browseAllAccessesTo:from: do: [ :ref :env | varRef := ref ];
		when: #browseAllSendersOf: do: [ :aLiteral | globalRef := aLiteral key ];
		when: #browseAllReferencesTo: do: [ :aSelector | selectorRef := aSelector ].
	command context
		when: #selectedTextOrLine do: [ selectedTextOrLine ].

	varRef := globalRef := selectorRef := nil.
	selectedTextOrLine := 'Object'.
	command execute.
	self assert: globalRef equals: #Object
]

{ #category : #tests }
SpBrowseSendersCommandTest >> testExecuteKeywordSelector [
	<expectedFailure>
	| selectedTextOrLine varRef globalRef selectorRef |

	command context systemNavigation
		when: #browseAllAccessesTo:from: do: [ :ref :env | varRef := ref ];
		when: #browseAllSendersOf: do: [ :aLiteral | globalRef := aLiteral key ];
		when: #browseAllReferencesTo: do: [ :aSelector | selectorRef := aSelector ].
	command context
		when: #selectedTextOrLine do: [ selectedTextOrLine ].
	
	varRef := globalRef := selectorRef := nil.
	selectedTextOrLine := 'addItemLabeled: ''Some string'' do:'.
	command execute.
	self assert: selectorRef equals: #addItemLabeled:do:.
]

{ #category : #tests }
SpBrowseSendersCommandTest >> testExecuteReceiverPlusKeywordSelector [
	| selectedTextOrLine varRef globalRef selectorRef |

	command context systemNavigation
		when: #browseAllAccessesTo:from: do: [ :ref :env | varRef := ref ];
		when: #browseAllSendersOf: do: [ :aLiteral | globalRef := aLiteral key ];
		when: #browseAllReferencesTo: do: [ :aSelector | selectorRef := aSelector ].
	command context
		when: #selectedTextOrLine do: [ selectedTextOrLine ].

	varRef := globalRef := selectorRef := nil.
	selectedTextOrLine := 'self initialize: 42'.
	command execute.
	self assert: selectorRef equals: #initialize:.

	varRef := globalRef := selectorRef := nil.
	selectedTextOrLine := 'self initialize: 42 and: other'.
	command execute.
	self assert: selectorRef equals: #initialize:and:

]

{ #category : #tests }
SpBrowseSendersCommandTest >> testExecuteReceiverPlusUnarySelector [
	| selectedTextOrLine varRef globalRef selectorRef |

	command context systemNavigation
		when: #browseAllAccessesTo:from: do: [ :ref :env | varRef := ref ];
		when: #browseAllSendersOf: do: [ :aLiteral | globalRef := aLiteral key ];
		when: #browseAllReferencesTo: do: [ :aSelector | selectorRef := aSelector ].
	command context
		when: #selectedTextOrLine do: [ selectedTextOrLine ].

	varRef := globalRef := selectorRef := nil.
	selectedTextOrLine := 'self initialize'.
	command execute.
	self assert: selectorRef equals: #initialize
]

{ #category : #tests }
SpBrowseSendersCommandTest >> testExecuteUnarySelector [
	| selectedTextOrLine varRef globalRef selectorRef |

	command context systemNavigation
		when: #browseAllAccessesTo:from: do: [ :ref :env | varRef := ref ];
		when: #browseAllSendersOf: do: [ :aLiteral | globalRef := aLiteral key ];
		when: #browseAllReferencesTo: do: [ :aSelector | selectorRef := aSelector ].
	command context
		when: #selectedTextOrLine do: [ selectedTextOrLine ].

	varRef := globalRef := selectorRef := nil.
	selectedTextOrLine := 'initialize'.
	command execute.
	self assert: selectorRef equals: #initialize
]
