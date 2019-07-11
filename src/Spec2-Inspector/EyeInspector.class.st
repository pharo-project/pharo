"
To do a specific inspector subclass this and override 	EyeInspector>>addSpecialFields
Then on your object override
	Object>>inspectorClass
so it returns your new inspector
"
Class {
	#name : #EyeInspector,
	#superclass : #EyeAbstractInspector,
	#instVars : [
		'list',
		'description'
	],
	#category : #'Spec2-Inspector-Base'
}

{ #category : #inspecting }
EyeInspector class >> basicInspect: anObject [ 
	^ self new 
		inspect: anObject;
		openWithSpec: #inspectorSpec;
		yourself
]

{ #category : #spec }
EyeInspector class >> debuggerSpec [
	<spec: #default>
	^ SpPanedLayout newVertical
		position: 70 percent;
		add: #list;
		add: #description;
		yourself
]

{ #category : #inspecting }
EyeInspector class >> explore: anObject [ 
	EyeInspectorNavigator openExplorer: (self inspector: anObject).
	^ anObject
]

{ #category : #inspecting }
EyeInspector class >> inspect: anObject [ 
	EyeInspectorNavigator openInspector: (self inspector: anObject).
	^ anObject
]

{ #category : #inspecting }
EyeInspector class >> inspect: anObject label: aString [
	(EyeInspectorNavigator openInspector: (self inspector: anObject)) window title: aString.
	^ anObject

]

{ #category : #inspecting }
EyeInspector class >> inspector: anObject [ 
	^ anObject class inspectorClass new
		inspect: anObject;
		yourself
]

{ #category : #spec }
EyeInspector class >> inspectorSpec [
	<spec>
	^ SpPanedLayout newVertical
		position: 70 percent;
		add:
			(SpPanedLayout newHorizontal
				position: 40 percent;
				add: #list;
				add: #description;
				yourself);
		add: #text;
		yourself
]

{ #category : #accessing }
EyeInspector class >> label [
	^ 'Default Inspector'
]

{ #category : #'tools registry' }
EyeInspector class >> registerToolsOn: registry [
	"Add ourselves to registry. See [Smalltalk tools]" 
	registry 
		register: self as: #inspector;
		register: self as: #basicInspector.
]

{ #category : #accessing }
EyeInspector class >> showAllInstVars [
	^ false
]

{ #category : #accessing }
EyeInspector class >> showClass [
	^ false
]

{ #category : #list }
EyeInspector >> addAllInstVars: elements [
	"Add AllInstVars to the list of elements you can access in the inspector"

	elements add: (AllInstVarsEyeElement host: self object).
]

{ #category : #list }
EyeInspector >> addClass: elements [
	"Add class to the list of elements you can access in the inspector"

	elements add: (ClassEyeElement host: self object).
]

{ #category : #list }
EyeInspector >> addInstancesVariable: elements [
	"Add all the instance variables to the list of elements you can access in the inspector, set them so that you can modify and save them"

	self object class allInstVarNames do: [:name |
		elements add: (InstanceVariableEyeElement host: self object instVarName: name) ]
]

{ #category : #list }
EyeInspector >> addSelf: elements [
	"Add self to the list of elements you can access in the inspector"

	elements add: (SelfEyeElement host: self object).
]

{ #category : #list }
EyeInspector >> addSpecialFields: elements [
	"Override in subclass to add fields to the inspector. You need to choose the appropriate sublcass of AbstractEyeElement"
	
	"ex :
	elements add: (AbstractEyeElement host: self object)"
]

{ #category : #list }
EyeInspector >> addVariableFields: elements [
	"add the variable fields to the inspector. Shorten the list for very long collection (cf limit1 and limit2), set them so that you can modify and save them"

	self variableFieldsToShow do: [ :index |
		elements add: (BasicIndexedEyeElement host: self object index: index) ]
]

{ #category : #private }
EyeInspector >> canDiscardEdits [

	^ true
]

{ #category : #actions }
EyeInspector >> copySelectedItemAccessorCode [
	self hasSelectedObject
		ifFalse: [ ^ self ].
	self selectedElement copyAccessorCode.
]

{ #category : #accessing }
EyeInspector >> description [
	^ description
]

{ #category : #accessing }
EyeInspector >> description: anObject [
	description := anObject
]

{ #category : #accessing }
EyeInspector >> doItContext [
	"Define a context where the DoIt method of the inspector should be done. To be overrriden in subclasses"
	^ nil
]

{ #category : #accessing }
EyeInspector >> elements [
	^ self list items
]

{ #category : #inspecting }
EyeInspector >> explore: anObject [ 
	"This is called to open a new ObjectExplorer on the object"

	^self class explore: anObject
]

{ #category : #list }
EyeInspector >> generateElements [
	"generate the collection of elements to show in the inspector for the object inspected"

	| elements |
	elements := OrderedCollection new.

	self addSelf: elements.
	self showClass ifTrue: [ self addClass: elements ].
	self showAllInstVars ifTrue: [ self addAllInstVars: elements ].
	self addSpecialFields: elements.
	self addInstancesVariable: elements.
	self addVariableFields: elements.
	
	^ elements
]

{ #category : #initialization }
EyeInspector >> initializePresenter [
	"initialize the event handlings"

	super initializePresenter.
	description
		whenTextIsAcceptedDo: [ :newText | 
			self saveElement: newText.
			self refreshDescription ]
]

{ #category : #initialization }
EyeInspector >> initializeShortcuts [
	"initialize the inspector' shortcuts with the dictionary defined in self shortCuts"
	self shortCuts keysAndValuesDo: [ :key :value |
		self list bindKeyCombination: key toAction: value ]

]

{ #category : #initialization }
EyeInspector >> initializeWidgets [
	super initializeWidgets.
	description := self newText
]

{ #category : #inspecting }
EyeInspector >> inspect: anObject [
	"This is called to update the inspector model with a new object, updates the UI"

	self object: anObject.
	self list selectIndex: 1.
	self refresh
]

{ #category : #menu }
EyeInspector >> inspectionMenu: aMenu [
	self hasSelectedObject ifFalse: [ ^ aMenu ].
	^ super inspectionMenu: aMenu.
	
]

{ #category : #'event-handling' }
EyeInspector >> keepScrollPositionOf: specHolder during: aBlock [
	| scrollValue |
	specHolder adapter ifNil: [ ^ aBlock value ].
	scrollValue := description scrollValue.
	aBlock value.
	description scrollValue: scrollValue
]

{ #category : #private }
EyeInspector >> labelFor: anEyeElement [
	^ anEyeElement label
]

{ #category : #list }
EyeInspector >> limit1 [
	"This is the max index shown before skipping to the 
	last limit2 elements of very long arrays"
	
	^ 100
]

{ #category : #list }
EyeInspector >> limit2 [
	"This is the number of elements to show at the end of very long arrays"
	
	^ 10
]

{ #category : #accessing }
EyeInspector >> list [
	^ list
		ifNil: [ list := self newList
				activateOnDoubleClick;
				whenSelectionChangedDo: [ :selection | [ :item | self refreshDescription: item ] cull: selection selectedItem ];
				whenActivatedDo: [ self diveIntoSelectedObject ];
				yourself ]
]

{ #category : #'event-handling' }
EyeInspector >> objectChanged [
	"triggered when the inspected object is changed"

	self updateList.
	self text doItReceiver: self object.
	self description doItReceiver: self object.
	self text doItContext: self doItContext.
	self description doItContext: self doItContext.
	self text ifNotNil: [ :w | w behavior: self objectClass ]
]

{ #category : #accessing }
EyeInspector >> objectClass [

	^ self object class
]

{ #category : #accessing }
EyeInspector >> objectVariableSize [
	^ self object basicSize
]

{ #category : #'event-handling' }
EyeInspector >> printDescription [
	^ description text
]

{ #category : #'event-handling' }
EyeInspector >> printDescription: item [
	self refreshDescription: item
]

{ #category : #accessing }
EyeInspector >> printTimeout [
	^ 10 seconds
]

{ #category : #'event-handling' }
EyeInspector >> refresh [
	"refresh the object in the inspector, updating his modified instance variables"

	description hasUnacceptedEdits 
		ifTrue: [  ^ self ].
	description getSelection size = 0 
		ifTrue: [ self refreshDescription; updateList ]
]

{ #category : #'event-handling' }
EyeInspector >> refreshDescription [
	self refreshDescription: self selectedElement
]

{ #category : #'event-handling' }
EyeInspector >> refreshDescription: item [
	"When you select an element in the list of the inspector, print on the description textModel the element selected"
	| newDescription |
	item ifNil: [ ^ self ].
	
	newDescription := ([ item description ] on: Error do: [ 'Error while printing ', item accessorCode ]).
	
	description text = newDescription
		ifTrue: [ ^ self ].
		
	self 
		keepScrollPositionOf: description 
		during: [ description text: newDescription ].
]

{ #category : #'event-handling' }
EyeInspector >> refreshPrintDescription [
	self printDescription: self selectedElement
]

{ #category : #'event-handling' }
EyeInspector >> saveElement [
	"When you press Cmd+s on the description TextPresenter, you can save the new value in the inspected object"

	|newValue|
		
	newValue := description doItReceiver class compiler		
		source: description text readStream;
		context: nil "should be object in case of methodContext";
		receiver:  description doItReceiver;
		failBlock: [^ self];
		evaluate.
		
	self selectedElement save: newValue.
	self objectChanged
]

{ #category : #'event-handling' }
EyeInspector >> saveElement: newText [
	"When you press Cmd+s on the description TextPresenter, you can save the new value in the inspected object"

	|newValue|
		
	newValue := description doItReceiver class compiler
		source: newText;
		context: self doItContext;
		receiver: description doItReceiver;
		failBlock: [^ self];
		evaluate.
	
	self selectedElement save: newValue.
	self objectChanged.
]

{ #category : #accessing }
EyeInspector >> selectedElement [
	^ self list selection selectedItem
		ifNil:
			[ "sometimes the new list model desynchronize its selected index from its selected element,
		this mean that the inspector evaluates 'list setSelectedIndex: 1' but 'list selectedItem' still returns nil.
		This code fixes this case, that is triggered for example when you do Cmd+c in an ObjectExplorer."
			self list selectedIndex
				ifNotNil: [ :currentIndex | 
					currentIndex = 0
						ifTrue: [ ^ nil ].
					self list listItems
						ifNotEmpty: [ :aList | aList at: currentIndex ] ] ]
]

{ #category : #accessing }
EyeInspector >> selectedIndex [
	^ self list selectedIndex
]

{ #category : #accessing }
EyeInspector >> selectedIndex: integer [
	^ self list setSelectedIndex: integer
]

{ #category : #accessing }
EyeInspector >> selectedObject [
	^self selectedElement value
]

{ #category : #accessing }
EyeInspector >> selectedObject: anObject [
	"Sets the selectedElement of the inspector to anObject. If anObject is not in the list, then sets the selectedElement to 1"

	self list setSelectedIndex: 1.
	self list listItems withIndexDo: [ :item :index | 
		item value == anObject 
			ifTrue: [ self list setSelectedIndex: index ] ]
]

{ #category : #initialization }
EyeInspector >> shortCuts [
	| shortCuts |
	shortCuts := super shortCuts.

	shortCuts at: $c meta put: [ self copySelectedItemAccessorCode ].
	
	^ shortCuts
]

{ #category : #testing }
EyeInspector >> showAllInstVars [
	^ self class showAllInstVars
]

{ #category : #testing }
EyeInspector >> showClass [
	^ self class showClass
]

{ #category : #initialization }
EyeInspector >> step [
	self refresh
]

{ #category : #private }
EyeInspector >> takeKeyboardFocus [
	^ self list takeKeyboardFocus
]

{ #category : #'event-handling' }
EyeInspector >> updateList [
	"update the list of elements displayed according to the new object"

	| elements |
	"	self haltOnce."
	elements := self generateElements.
	self list items = elements
		ifTrue: [ ^ self ].
	"first reset the items to make sure we don't interfere with the display block"
	"self list items: #()."
	self list displayBlock: [ :eyeElement | self labelFor: eyeElement ].
	self list items: elements.

	"handle when last item of list is removed"
	self list selection selectedIndex > self list items size
		ifTrue: [ self list setSelectedIndex: self list listItems size ].

	"handle when selected dictionary key is removed"
	self list selectIndex: self list selection selectedIndex.
	self list menu: [ :aMenu | self inspectionMenu: aMenu ]
]

{ #category : #list }
EyeInspector >> variableFieldsToShow [
	"Answers the indexes of the variable fields of the object to show.
	Shorten the list for very long collection (cf limit1 and limit2)"

	|bSize|
	
	bSize := self objectVariableSize.
	
	^ bSize <= (self limit1 + self limit2)
			ifTrue: [1 to: bSize]
			ifFalse: [(1 to: self limit1) , (bSize - self limit2 + 1 to: bSize)].
]
