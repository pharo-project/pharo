"
MCCodeTool is an abstract superclass for those Monticello browsers that display code.
It contains copies of the various CodeHolder methods that perform the various menu operations in the method list.

"
Class {
	#name : #MCCodeTool,
	#superclass : #MCTool,
	#instVars : [
		'items'
	],
	#category : 'MonticelloGUI'
}

{ #category : #menus }
MCCodeTool >> browseMessages [
	"Present a menu of the currently selected message, as well as all messages sent by it.  Open a message set browser of all implementors of the selector chosen."

	self systemNavigation browseAllImplementorsOf: (self selectedMessageName ifNil: [ ^nil ])
]

{ #category : #menus }
MCCodeTool >> browseMethodFull [
	"Create and schedule a full Browser and then select the current class and message."

	| myClass |
	(myClass := self selectedClassOrMetaClass) ifNotNil:
		[Smalltalk tools browser openOnClass: myClass selector: self selectedMessageName]
]

{ #category : #menus }
MCCodeTool >> browseSendersOfMessages [
	"Present a menu of the currently selected message, as well as all messages sent by it.  Open a message set browser of all senders of the selector chosen."

	self systemNavigation browseAllSendersOf: (self selectedMessageName ifNil: [ ^nil ])
]

{ #category : #menus }
MCCodeTool >> browseVersions [
	"Create and schedule a message set browser on all versions of the 
	currently selected message selector."
	
	Smalltalk tools versionBrowser  
		browseVersionsForClass:  self selectedClassOrMetaClass 
		selector: self selectedMessageName
		
]

{ #category : #menus }
MCCodeTool >> classHierarchy [
	"Create and schedule a class list browser on the receiver's hierarchy."

	self systemNavigation 
		browseHierarchy: self selectedClassOrMetaClass
		selector: self selectedMessageName	"OK if nil"
]

{ #category : #menus }
MCCodeTool >> classListMenu: aMenu [ 

	^aMenu addList: #(
		-
		('Browse full (b)'			browseMethodFull)
		('Browse hierarchy (h)'		classHierarchy)
		-
		('Show hierarchy'			methodHierarchy))
]

{ #category : #menus }
MCCodeTool >> copySelector [
	"Copy the selected selector to the clipboard"

	| selector |
	(selector := self selectedMessageName) ifNotNil:
		[Clipboard clipboardText: selector asString]
]

{ #category : #menus }
MCCodeTool >> fileOutMessage [
	"Put a description of the selected message on a file"

	| fileName |
	self selectedMessageName ifNotNil:
		[Cursor write showWhile:
			[self selectedClassOrMetaClass fileOutMethod: self selectedMessageName].
		 ^self].
	items isEmpty ifTrue:
		[^self].
	fileName := UIManager default request: 'File out on which file?' initialAnswer: 'methods'.
	Cursor write showWhile:
		[| internalStream |
		internalStream := WriteStream on: (String new: 1000).
		internalStream header; timeStamp.
		items do:
			[:patchOp|
			patchOp definition isMethodDefinition ifTrue:
				[(patchOp definition actualClass notNil
				  and: [patchOp definition actualClass includesSelector: patchOp definition selector])
					ifTrue:
						[patchOp definition actualClass
							printMethodChunk: patchOp definition selector
							on: internalStream ]
					ifFalse:
						[internalStream nextChunkPut: patchOp definition className, ' removeSelector: ', patchOp definition selector printString]].
			patchOp definition isClassDefinition ifTrue:
				[patchOp definition actualClass
					ifNotNil:
						[internalStream nextChunkPut: patchOp definition actualClass definition.
						 patchOp definition comment ifNotNil:
							[patchOp definition actualClass organization
								putCommentOnFile: internalStream
								forClass: patchOp definition actualClass]]
					ifNil:
						[internalStream nextChunkPut: patchOp definition className, ' removeFromSystem']]].
		CodeExporter writeSourceCodeFrom: internalStream baseName: fileName isSt: true]
]

{ #category : #menus }
MCCodeTool >> methodHierarchy [
	"Create and schedule a method browser on the hierarchy of implementors."

	self systemNavigation methodHierarchyBrowserForClass: self selectedClassOrMetaClass
		selector: self selectedMessageName
]

{ #category : #menus }
MCCodeTool >> methodListKey: aKeystroke from: aListMorph [ 
	aKeystroke caseOf: {
		[$b] -> [self browseMethodFull].
		[$h] -> [self classHierarchy].
		[$o] -> [self fileOutMessage].
		[$c] -> [self copySelector].
		[$n] -> [self browseSendersOfMessages].
		[$m] -> [self browseMessages].
		[$i] -> [self methodHierarchy].
		[$v] -> [self browseVersions]}
		 otherwise: []
]

{ #category : #menus }
MCCodeTool >> methodListMenu: aMenu [
	"Build the menu for the selected method, if any."
	
	self selectedMessageName 
		ifNil: [ items notEmpty ifTrue: [aMenu add: 'FileOut (o)'	selector: #fileOutMessage]]
		ifNotNil: [
			aMenu addList:#(
				('Browse full (b)' 						browseMethodFull)
				('Browse hierarchy (h)'					classHierarchy)
				-
				('FileOut (o)'							fileOutMessage)
				('Copy selector (c)'						copySelector)).
			aMenu addList: #(
				-
				('Browse senders (n)'						browseSendersOfMessages)
				('Browse implementors (m)'					browseMessages)
				('Inheritance (i)'						methodHierarchy)
				('Versions (v)'							browseVersions)) ].
	^ aMenu

]

{ #category : #subclassresponsibility }
MCCodeTool >> selectedClass [
	"Answer the class that is selected, or nil"
	self subclassResponsibility
]

{ #category : #subclassresponsibility }
MCCodeTool >> selectedClassOrMetaClass [
	"Answer the class that is selected, or nil"
	self subclassResponsibility
]

{ #category : #subclassresponsibility }
MCCodeTool >> selectedMessageCategoryName [
	"Answer the method category of the method that is selected, or nil"
	self subclassResponsibility
]

{ #category : #subclassresponsibility }
MCCodeTool >> selectedMessageName [
	"Answer the name of the selected message"
	self subclassResponsibility
]
