Extension { #name : #CompiledMethod }

{ #category : #'*GT-SpotterExtensions-Core' }
CompiledMethod >> spotterActDefault [
	^ self browse
]

{ #category : #'*GT-SpotterExtensions-Core' }
CompiledMethod >> spotterForBytecodesFor: aStep [
	<spotterOrder: 15>
	aStep listProcessor
		title: 'Bytecode';
		allCandidates: [ self symbolicBytecodes ];
		itemName: #printString;
		filter: GTFilterSubstring
]

{ #category : #'*GT-SpotterExtensions-Core' }
CompiledMethod >> spotterForImplementorsFor: aStep [
	<spotterOrder: 10>
	aStep listProcessor
			title: 'Implementors';
			allCandidates: [ self implementors collect: #compiledMethod ];
			filter: GTFilterSubstring;
			keyBinding: $m command
]

{ #category : #'*GT-SpotterExtensions-Core' }
CompiledMethod >> spotterForMessagesFor: aStep [
	<spotterOrder: 12>
	aStep listProcessor
			title: 'Messages';
			allCandidates: [ self messages asSortedCollection collect: [:each | GTSelector new name: each ] ];
			filter: GTFilterSubstring
]

{ #category : #'*GT-SpotterExtensions-Core' }
CompiledMethod >> spotterForSendersFor: aStep [
	<spotterOrder: 11>
	aStep listProcessor
			title: 'Senders';
			allCandidates: [ self senders collect: #compiledMethod ];
			filter: GTFilterSubstring;
			keyBinding: $n command
]

{ #category : #'*GT-SpotterExtensions-Core' }
CompiledMethod >> spotterItemsFor: aStep [
	"do nothing for compiled method"
]

{ #category : #'*GT-SpotterExtensions-Core' }
CompiledMethod >> spotterMethodClassFor: aStep [
	<spotterOrder: 50>
	aStep listProcessor
			allCandidates: [  {self methodClass }];
			title: 'Class';
			filter: GTFilterSubstring;
			itemIcon: #systemIcon
]

{ #category : #'*GT-SpotterExtensions-Core' }
CompiledMethod >> spotterPreviewCodeIn: aComposite [
	<spotterPreview: 10>
	aComposite pharoMethod
		title: [ self gtDisplayString ];
		display: [:compiledMethod | 
			' ', compiledMethod sourceCode];
		smalltalkClass: [self methodClass];
		entity: self.
		
	self flag: 'move "entity: self" somewhere else, maybe'
]

{ #category : #'*GT-SpotterExtensions-Core' }
CompiledMethod >> spotterSelectFor: aStep [
	" ignore :) "
]
