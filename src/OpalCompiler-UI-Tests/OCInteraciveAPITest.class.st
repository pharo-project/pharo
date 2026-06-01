Class {
	#name : 'OCInteraciveAPITest',
	#superclass : 'TestCase',
	#instVars : [
		'node',
		'text',
		'requestor',
		'notifications',
		'repairBlock',
		'interactive'
	],
	#category : 'OpalCompiler-UI-Tests',
	#package : 'OpalCompiler-UI-Tests'
}

{ #category : 'interactive error protocol' }
OCInteraciveAPITest >> correctFrom: anInteger to: anInteger2 with: aString [ 

	text := text copyReplaceFrom: anInteger to: anInteger2 with: aString
]

{ #category : 'tests' }
OCInteraciveAPITest >> declareAsTemp [

	(OCCodeReparator new node: node)
		requestor: requestor;
		declareTempAndPaste: node name.
	^ true
]

{ #category : 'accessing' }
OCInteraciveAPITest >> interactive [

	^ interactive
]

{ #category : 'accessing' }
OCInteraciveAPITest >> interactive: anObject [

	interactive := anObject
]

{ #category : 'accessing' }
OCInteraciveAPITest >> node: aNode [

	node := aNode
]

{ #category : 'interactive error protocol' }
OCInteraciveAPITest >> notify: errorMessage at: position in: sourceCode [

	notifications add: { errorMessage . position . sourceCode }
]

{ #category : 'correcting' }
OCInteraciveAPITest >> repair [
	
	^ repairBlock value: self
]

{ #category : 'accessing' }
OCInteraciveAPITest >> requestor: anOCInteraciveAPITest [ 
	
	requestor := anOCInteraciveAPITest
]

{ #category : 'running' }
OCInteraciveAPITest >> setUp [

	super setUp.
	notifications := OrderedCollection new.

	"By default it is interactive, which will call a reparator"
	interactive := true.
	
	repairBlock := [ :reparator | self error: 'define a repairBlock for the test' ]
]

{ #category : 'tests' }
OCInteraciveAPITest >> testCodeWithoutInteractiveReparationSendsNotificationToRequestor [

	| method |
	
	"I set myself as not interactive, this will avoid the automatic reparation and launch the old notify process"
	interactive := false.
	
	text := 'foo "x and y are not declared" x + y'.
	method := OCInteractiveAPI new
		          undeclaredVariableReparator: self;
		          compile: text notifying: self in: self class.

	self assert: notifications asArray equals: {
			#('Undeclared variable named x in foo ->' 32 'foo "x and y are not declared" x + y').
			#('Undeclared variable named y in foo ->' 36 'foo "x and y are not declared" x + y') }
]

{ #category : 'tests' }
OCInteraciveAPITest >> testUndeclaredsAreDefined [

	| method |
	repairBlock := [ :reparator | reparator declareAsTemp ].

	text := 'foo "x and y are not declared" x + y'.
	method := OCInteractiveAPI new
		          undeclaredVariableReparator: self;
		          compile: text notifying: self in: self class.

	self assert: text equals: 'foo "x and y are not declared" | x y |
x + y'
]

{ #category : 'accessing' }
OCInteraciveAPITest >> text [
	
	^ text
]
