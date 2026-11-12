"
I am the context of the method editor (ClyMethodCodeEditorToolMorph): the source code of a method, with the node selected in it.
I answer the edited method as the selected method (#selectedMethods, #lastSelectedMethod), so the commands of methods also apply in the editor, and the commands declared on me apply to method source code, such as the debug points of the left bar of the editor.
"
Class {
	#name : 'ClyMethodSourceCodeContext',
	#superclass : 'ClySourceCodeContext',
	#category : 'Calypso-SystemTools-Core-Editors-Methods',
	#package : 'Calypso-SystemTools-Core',
	#tag : 'Editors-Methods'
}

{ #category : 'commands' }
ClyMethodSourceCodeContext class >> calypsoSystemToolsCoreCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap shortcuts
		add: SycMoveTempDefinitionCommand key: $o meta;
		add: SycRenameArgOrTempCommand2 key: $r meta;
		add: SycRenameMessageCommand key: $r meta;
		add: SycShowLocalSendersCommand key: $y shift meta
]

{ #category : 'testing' }
ClyMethodSourceCodeContext >> isMethodSelected [

	self selectedSourceNode ifNil: [ ^ false ].
	^ self selectedSourceNode isMethod
]

{ #category : 'testing' }
ClyMethodSourceCodeContext >> isMethodSourceContext [

	^ true
]

{ #category : 'testing' }
ClyMethodSourceCodeContext >> isTestMethodSelected [

	| method |
	method := self selectedMethod.
	^ method notNil and: [ method selector isTestSelector ]
]

{ #category : 'tool controlling' }
ClyMethodSourceCodeContext >> lastSelectedClass [ 
	^ self selectedMethod methodClass
]

{ #category : 'selection' }
ClyMethodSourceCodeContext >> lastSelectedMethod [
	^self selectedMethod
]

{ #category : 'selection' }
ClyMethodSourceCodeContext >> selectedMethod [
	^tool editingMethod
]

{ #category : 'selection' }
ClyMethodSourceCodeContext >> selectedMethods [
	^{self selectedMethod}
]

{ #category : 'tool controlling' }
ClyMethodSourceCodeContext >> showMessage: aMessageDescription renamedTo: newSelector [
	"here we expect only messages which was created from ast-nodes"
	tool editingMethod selector == aMessageDescription selector
		ifTrue: [ tool switchToMethod: tool methodClass >> newSelector]
		ifFalse: [ tool selectAnySelector: { newSelector } ]
]
