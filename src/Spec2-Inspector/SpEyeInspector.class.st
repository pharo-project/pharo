"
To do a specific inspector subclass this and override 	EyeInspector>>addSpecialFields
Then on your object override
	Object>>inspectorClass
so it returns your new inspector
"
Class {
	#name : #SpEyeInspector,
	#superclass : #SpEyeAbstractInspector,
	#instVars : [
		'list',
		'description'
	],
	#category : #'Spec2-Inspector-Base'
}

{ #category : #inspecting }
SpEyeInspector class >> basicInspect: anObject [ 
	^ self new 
		inspect: anObject;
		openWithSpec: #inspectorSpec;
		yourself
]

{ #category : #spec }
SpEyeInspector class >> debuggerSpec [
	<spec:#default>
	
	^ SpLayout composed
		add: #list origin: 0@0 corner: 0.33@1;
		addVSplitter;
		add: #description origin: 0.33@0 corner: 1@1;
		yourself
]

{ #category : #spec }
SpEyeInspector class >> debuggerSpecSlow [
	<spec>
	
	^ SpLayout composed
		newRow: [:row | 
			row 
				add: #list width: 135;
				add: #description ];
		yourself
]

{ #category : #inspecting }
SpEyeInspector class >> explore: anObject [ 
	SpInspectorNavigator openExplorer: (self inspector: anObject).
	^ anObject
]

{ #category : #inspecting }
SpEyeInspector class >> inspect: anObject [ 
	SpInspectorNavigator openInspector: (self inspector: anObject).
	^ anObject
]

{ #category : #inspecting }
SpEyeInspector class >> inspect: anObject label: aString [
	(SpInspectorNavigator openInspector: (self inspector: anObject)) window title: aString.
	^ anObject

]

{ #category : #inspecting }
SpEyeInspector class >> inspector: anObject [ 
	^ anObject class inspectorClass new
		inspect: anObject;
		yourself
]

{ #category : #spec }
SpEyeInspector class >> inspectorSpec [
	<spec>
	
	^ SpLayout composed
		newRow: [ :r |
			r 
				newColumn: #list right: 0.6;
				addSplitter;
				newColumn: #description left: 0.4 ]
		bottom: 0.3;
		addHSplitter;
		newRow: #text top: 0.7;
		yourself
]

{ #category : #accessing }
SpEyeInspector class >> label [
	^ 'Default Inspector'
]

{ #category : #'tools registry' }
SpEyeInspector class >> registerToolsOn: registry [
	"Add ourselves to registry. See [Smalltalk tools]" 
	registry 
		register: self as: #inspector;
		register: self as: #basicInspector.
]

{ #category : #accessing }
SpEyeInspector class >> showAllInstVars [
	^ false
]

{ #category : #accessing }
SpEyeInspector class >> showClass [
	^ false
]

{ #category : #list }
SpEyeInspector >> addAllInstVars: elements [
	"Add AllInstVars to the list of elements you can access in the inspector"

	elements add: (SpAllInstVarsEyeElement host: self object).
]

{ #category : #list }
SpEyeInspector >> addClass: elements [
	"Add class to the list of elements you can access in the inspector"

	elements add: (SpClassEyeElement host: self object).
]

{ #category : #list }
SpEyeInspector >> addInstancesVariable: elements [
	"Add all the instance variables to the list of elements you can access in the inspector, set them so that you can modify and save them"

	self object class allInstVarNames do: [:name |
		elements add: (SpInstanceVariableEyeElement host: self object instVarName: name) ]
]

{ #category : #list }
SpEyeInspector >> addSelf: elements [
	"Add self to the list of elements you can access in the inspector"

	elements add: (SpSelfEyeElement host: self object).
]

{ #category : #list }
SpEyeInspector >> addSpecialFields: elements [
	"Override in subclass to add fields to the inspector. You need to choose the appropriate sublcass of AbstractEyeElement"
	
	"ex :
	elements add: (AbstractEyeElement host: self object)"
]

{ #category : #list }
SpEyeInspector >> addVariableFields: elements [
	"add the variable fields to the inspector. Shorten the list for very long collection (cf limit1 and limit2), set them so that you can modify and save them"

	self variableFieldsToShow do: [ :index |
		elements add: (SpBasicIndexedEyeElement host: self object index: index) ]
]

{ #category : #private }
SpEyeInspector >> canDiscardEdits [

	^ true
]

{ #category : #actions }
SpEyeInspector >> copySelectedItemAccessorCode [
	self hasSelectedObject
		ifFalse: [ ^ self ].
	self selectedElement copyAccessorCode.
]

{ #category : #accessing }
SpEyeInspector >> description [
	^ description
]

{ #category : #accessing }
SpEyeInspector >> description: anObject [
	description := anObject
]

{ #category : #accessing }
SpEyeInspector >> doItContext [
	"Define a context where the DoIt method of the inspector should be done. To be overrriden in subclasses"
	^ nil
]

{ #category : #accessing }
SpEyeInspector >> elements [
	^ self list items
]

{ #category : #inspecting }
SpEyeInspector >> explore: anObject [ 
	"This is called to open a new ObjectExplorer on the object"

	^self class explore: anObject
]

{ #category : #list }
SpEyeInspector >> generateElements [
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
SpEyeInspector >> initializePresenter [
	"initialize the event handlings"

	super initializePresenter.
	description
		whenTextIsAcceptedDo: [ :newText | 
			self saveElement: newText.
			self refreshDescription ]
]

{ #category : #initialization }
SpEyeInspector >> initializeShortcuts [
	"initialize the inspector' shortcuts with the dictionary defined in self shortCuts"
	self shortCuts keysAndValuesDo: [ :key :value |
		self list bindKeyCombination: key toAction: value ]

]

{ #category : #initialization }
SpEyeInspector >> initializeWidgets [
	description := self newText
]

{ #category : #inspecting }
SpEyeInspector >> inspect: anObject [
	"This is called to update the inspector model with a new object, updates the UI"

	self object: anObject.
	self list selectIndex: 1.
	self refresh
]

{ #category : #menu }
SpEyeInspector >> inspectionMenu: aMenu [
	self hasSelectedObject ifFalse: [ ^ aMenu ].
	^ super inspectionMenu: aMenu.
	
]

{ #category : #'event-handling' }
SpEyeInspector >> keepScrollPositionOf: specHolder during: aBlock [
	| scrollValue |
	specHolder adapter ifNil: [ ^ aBlock value ].
	scrollValue := description scrollValue.
	aBlock value.
	description scrollValue: scrollValue
]

{ #category : #private }
SpEyeInspector >> labelFor: anEyeElement [
	^ anEyeElement label
]

{ #category : #list }
SpEyeInspector >> limit1 [
	"This is the max index shown before skipping to the 
	last limit2 elements of very long arrays"
	
	^ 100
]

{ #category : #list }
SpEyeInspector >> limit2 [
	"This is the number of elements to show at the end of very long arrays"
	
	^ 10
]

{ #category : #accessing }
SpEyeInspector >> list [
	^ list
		ifNil: [ list := self newList
				activateOnDoubleClick;
				whenSelectionChangedDo: [ :selection | [ :item | self refreshDescription: item ] cull: selection selectedItem ];
				whenActivatedDo: [ self diveIntoSelectedObject ];
				yourself ]
]

{ #category : #'event-handling' }
SpEyeInspector >> objectChanged [
	"triggered when the inspected object is changed"

	self updateList.
	self text doItReceiver: self object.
	self description doItReceiver: self object.
	self text doItContext: self doItContext.
	self description doItContext: self doItContext.
	self text ifNotNil: [ :w | w behavior: self objectClass ]
]

{ #category : #accessing }
SpEyeInspector >> objectClass [

	^ self object class
]

{ #category : #accessing }
SpEyeInspector >> objectVariableSize [
	^ self object basicSize
]

{ #category : #'event-handling' }
SpEyeInspector >> printDescription [
	^ description text
]

{ #category : #'event-handling' }
SpEyeInspector >> printDescription: item [
	self refreshDescription: item
]

{ #category : #accessing }
SpEyeInspector >> printTimeout [
	^ 10 seconds
]

{ #category : #'event-handling' }
SpEyeInspector >> refresh [
	"refresh the object in the inspector, updating his modified instance variables"

	description hasUnacceptedEdits 
		ifTrue: [  ^ self ].
	description getSelection size = 0 
		ifTrue: [ self refreshDescription; updateList ]
]

{ #category : #'event-handling' }
SpEyeInspector >> refreshDescription [
	self refreshDescription: self selectedElement
]

{ #category : #'event-handling' }
SpEyeInspector >> refreshDescription: item [
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
SpEyeInspector >> refreshPrintDescription [
	self printDescription: self selectedElement
]

{ #category : #'event-handling' }
SpEyeInspector >> saveElement [
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
SpEyeInspector >> saveElement: newText [
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
SpEyeInspector >> selectedElement [
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
SpEyeInspector >> selectedIndex [
	^ self list selectedIndex
]

{ #category : #accessing }
SpEyeInspector >> selectedIndex: integer [
	^ self list setSelectedIndex: integer
]

{ #category : #accessing }
SpEyeInspector >> selectedObject [
	^self selectedElement value
]

{ #category : #accessing }
SpEyeInspector >> selectedObject: anObject [
	"Sets the selectedElement of the inspector to anObject. If anObject is not in the list, then sets the selectedElement to 1"

	self list setSelectedIndex: 1.
	self list listItems withIndexDo: [ :item :index | 
		item value == anObject 
			ifTrue: [ self list setSelectedIndex: index ] ]
]

{ #category : #initialization }
SpEyeInspector >> shortCuts [
	| shortCuts |
	shortCuts := super shortCuts.

	shortCuts at: $c meta put: [ self copySelectedItemAccessorCode ].
	
	^ shortCuts
]

{ #category : #testing }
SpEyeInspector >> showAllInstVars [
	^ self class showAllInstVars
]

{ #category : #testing }
SpEyeInspector >> showClass [
	^ self class showClass
]

{ #category : #initialization }
SpEyeInspector >> step [
	self refresh
]

{ #category : #private }
SpEyeInspector >> takeKeyboardFocus [
	^ self list takeKeyboardFocus
]

{ #category : #'event-handling' }
SpEyeInspector >> updateList [
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
SpEyeInspector >> variableFieldsToShow [
	"Answers the indexes of the variable fields of the object to show.
	Shorten the list for very long collection (cf limit1 and limit2)"

	|bSize|
	
	bSize := self objectVariableSize.
	
	^ bSize <= (self limit1 + self limit2)
			ifTrue: [1 to: bSize]
			ifFalse: [(1 to: self limit1) , (bSize - self limit2 + 1 to: bSize)].
]
