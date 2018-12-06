"
A SettingDeclaration is an handler for a writtable setting value. 
Its main responsibility is allow real value changing and to hold descriptive data about the setting. When the real value is changed, all declared listeners are notified. The standard event mechanism is used for that purpose. See #value: method which is call in order to change a setting value. Whatever object can declare its interest in the real value changes by sending #whenChangedSend:to: to a setting. See notification protocol.

For convenience, my olds values are registered in a list. This list is used by the SystemSettingBrowser in order to easily retrieve previous values.

The type instance variable is a Symbol. It is used by the SystemSettingBrowser in order to build the input widget of a setting.
It can be the name of the class or the name of a superclass of the value. In that case, the input widget is built by #settingInputWidgetForNode: implemented by the class which name is the value of the type instance variable. As an example, the input widget for a Boolean is built by Boolean class>>settingInputWidgetForNode:.

Type value is not limited to be a class name. It can also be whatever symbol. In that case, the selector corresponding to the message to send to the setting in order to build the inputWidget is built by SettingDeclaration>>localInputWidgetSelector. If the type instance variable is not set, then it is dynamically set to the class name of the value. 

See SettingManager comment for more explanations.

Instance Variables
	default:		<Object>
	getSelector:		<Object>
	ghostHelp:		<Object>
	setSelector:		<Object>
	type:		<Object>

default
	- xxxxx

getSelector
	- xxxxx

ghostHelp
	- xxxxx

setSelector
	- xxxxx

type
	- xxxxx

"
Class {
	#name : #SettingDeclaration,
	#superclass : #PragmaSetting,
	#instVars : [
		'getSelector',
		'setSelector',
		'type',
		'default',
		'ghostHelp'
	],
	#classVars : [
		'UniqueObject',
		'ValueListCache'
	],
	#category : #'System-Settings-Core-Base'
}

{ #category : #initialization }
SettingDeclaration class >> initialize [

	UniqueObject := Object new
]

{ #category : #'icon accessing' }
SettingDeclaration class >> nullifyForm [
	^ (Form
	extent: 16@16
	depth: 32
	fromArray: #( 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 463732736 4285815129 4285815129 4285815129 4285815129 4285815129 2292449280 463732736 0 0 0 0 0 0 0 1688469504 4285815129 4278190080 4278190080 4278190080 4278190080 4278190080 4280091395 4285815129 1688469504 0 0 0 0 0 1688469504 4285815129 4278190080 4278190080 4278190080 4278190080 4278190080 4278190080 4278190080 4278190080 4285815129 1688469504 0 0 0 463732736 4285815129 4278190080 4294967295 4294967295 4278190080 4278190080 4278190080 4278190080 4294967295 4294967295 4278190080 4285815129 463732736 0 0 4285815129 4278190080 4278190080 4294967295 4294967295 4294967295 4278190080 4278190080 4294967295 4294967295 4294967295 4278190080 4285815129 4285815129 0 0 4285815129 4278190080 4278190080 4278190080 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4278190080 4278190080 4278190080 4285815129 0 0 4285815129 4278190080 4278190080 4278190080 4278190080 4294967295 4294967295 4294967295 4294967295 4278190080 4278190080 4278190080 4278190080 4285815129 0 0 4285815129 4278190080 4278190080 4278190080 4278190080 4294967295 4294967295 4294967295 4294967295 4278190080 4278190080 4278190080 4278190080 4285815129 0 0 4285815129 4278190080 4278190080 4278190080 4294967295 4294967295 4294967295 4294967295 4294967295 4294967295 4278190080 4278190080 4278190080 4285815129 0 0 2292449280 4281927174 4280025345 4294967295 4294967295 4294967295 4278190080 4278190080 4294967295 4294967295 4294967295 4278190080 4283696903 4285815129 0 0 463732736 4285815129 4278190080 4294967295 4294967295 4278190080 4278190080 4278190080 4278190080 4294967295 4294967295 4278190080 4285815129 463732736 0 0 0 1688469504 4285815129 4278190080 4278190080 4278190080 4278190080 4278190080 4278190080 4278190080 4278190080 4285815129 1688469504 0 0 0 0 0 1688469504 4285815129 4285815129 4278190080 4278190080 4278190080 4278190080 4283696646 4285815129 1688469504 0 0 0 0 0 0 0 463732736 2292449280 4285815129 4285815129 4285815129 4285815129 2292449280 463732736 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	offset: 0@0)
]

{ #category : #'icon accessing' }
SettingDeclaration class >> smallCautionForm [
	^ (Form
	extent: 16@16
	depth: 32
	fromArray: #( 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 463732736 2292449280 3517186048 4154720256 4154720256 3517186048 2292449280 463732736 0 0 0 0 0 0 0 1688469504 4137943040 4292614939 4293600292 4293534756 4293534499 4293402913 4292483352 4137943040 1688469504 0 0 0 0 0 1688469504 4290185224 4293600292 4292284175 4291428866 4291166208 4291100672 4291297794 4292087053 4293271070 4290184967 1688469504 0 0 0 463732736 4137943040 4293534756 4291823367 4291231744 4291166208 4294967295 4294967295 4290969600 4290969600 4291429637 4293073948 4137943040 463732736 0 0 2292449280 4292614682 4292284175 4291231744 4291035136 4291035136 4294967295 4294967295 4291035136 4291035136 4290838528 4291824395 4292351253 2292449280 0 0 3517186048 4293468963 4291363330 4291166208 4291035136 4291035136 4294967295 4294967295 4291035136 4291035136 4290707456 4290838785 4292942105 3517186048 0 0 4154720256 4293534242 4291166208 4291100672 4291035136 4291035136 4294967295 4294967295 4291035136 4290707456 4290641920 4290641920 4292942104 4154720256 0 0 4154720256 4293468449 4291035136 4291035136 4291035136 4291035136 4291100672 4291100672 4291035136 4290641920 4290576384 4290510848 4292876056 4154720256 0 0 3517186048 4293271328 4291166722 4290969600 4291035136 4291035136 4291035136 4291035136 4291035136 4291035136 4290510848 4290576641 4292744726 3517186048 0 0 2292449280 4292417303 4291955981 4290838528 4291035136 4291035136 4294967295 4294967295 4291035136 4291035136 4290445312 4291430665 4292088338 2292449280 0 0 463732736 4137943040 4293139484 4291298565 4290707456 4290641920 4294967295 4294967295 4290510848 4290379776 4290905092 4292678933 4137943040 463732736 0 0 0 1688469504 4290184967 4293007898 4291693323 4290707713 4290510848 4290445312 4290511105 4291365129 4292678932 4290053638 1688469504 0 0 0 0 0 1688469504 4137943040 4292219924 4292876311 4292876055 4292810518 4292744726 4292088081 4137943040 1688469504 0 0 0 0 0 0 0 463732736 2292449280 3517186048 4154720256 4154720256 3517186048 2292449280 463732736 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
	offset: 0@0)
]

{ #category : #'cache accessing' }
SettingDeclaration class >> valueListCache [
	^ ValueListCache ifNil: [ValueListCache := WeakKeyDictionary new]
]

{ #category : #'cache accessing' }
SettingDeclaration class >> valueListFor: aSettingValue [
	| targetEntry |
	targetEntry := self valueListCache at: aSettingValue settingReceiver ifAbsentPut: [IdentityDictionary new].
	^ targetEntry at: aSettingValue getSelector ifAbsentPut: [OrderedCollection new].
]

{ #category : #comparing }
SettingDeclaration >> = other [
	self class = other class
		ifFalse: [^ false].
	self target == other target ifFalse: [^ false].
	self targetSelector = other targetSelector ifFalse: [^ false].
	self getSelector = other getSelector ifFalse: [^ false].
	self setSelector = other setSelector ifFalse: [^ false].
	^ true
	

]

{ #category : #visitor }
SettingDeclaration >> acceptSettings: aVisitor [ 
	^ aVisitor visitSettingDeclaration: self
]

{ #category : #'user interface' }
SettingDeclaration >> addToList: anItem [
	(self list includes: anItem)
		ifFalse: [self list size > self maxKept
				ifTrue: [self list removeFirst].
			self list add: anItem.
			self changed: #list]
]

{ #category : #accessing }
SettingDeclaration >> default [
	^ default 
]

{ #category : #accessing }
SettingDeclaration >> default: anObject [ 
	"initialize the default value"
	default := anObject.

]

{ #category : #'user interface' }
SettingDeclaration >> defaultIcon [
	^ (self type = #LogicalFont
		or: [ self realValue isKindOf: AbstractFont ])
		ifTrue: [ self iconNamed: #smallFontsIcon ]
		ifFalse: [ self iconNamed: #smallPaintIcon ]
]

{ #category : #accessing }
SettingDeclaration >> defaultValue [
	^ default value
]

{ #category : #export }
SettingDeclaration >> displayExportAction [

	Smalltalk tools workspace
		openContents: self startupAction storeString 
		label: self label
]

{ #category : #'user interface' }
SettingDeclaration >> emptyList [
	self list removeAll.
	self value ifNotNil: [self addToList: self realValue].
]

{ #category : #export }
SettingDeclaration >> exportSettingAction [
	| targetSymbol currentValue |
	
	targetSymbol := self target isSymbol
						ifTrue: [ self target ]
						ifFalse: [ self target name asSymbol ].
						
	currentValue := self targetSelector
						ifNil: [ (Smalltalk at: targetSymbol) perform: self getSelector ]
						ifNotNil: [:s | ((Smalltalk at: targetSymbol) perform: s) perform: self getSelector].
	
	^ (self hasDefault not or: [ (self default = currentValue) not ])
		ifTrue: [ self startupAction ]
		ifFalse: [ "We do nothing" nil ]
]

{ #category : #accessing }
SettingDeclaration >> getSelector [
	^ getSelector ifNil: [self name]
]

{ #category : #accessing }
SettingDeclaration >> getSelector: aSymbol [
	getSelector := aSymbol
]

{ #category : #accessing }
SettingDeclaration >> ghostHelp [
	^ ghostHelp 
]

{ #category : #accessing }
SettingDeclaration >> ghostHelp: aString [
	ghostHelp := aString
]

{ #category : #testing }
SettingDeclaration >> hasDefault [
	^ default ~~ UniqueObject
]

{ #category : #'user interface' }
SettingDeclaration >> hasEditableList [
	^ (self realValue isKindOf: Boolean) not
]

{ #category : #'user interface' }
SettingDeclaration >> hasPrivateListener [
	^ self hasActionForEvent: self privateChangeEvent
]

{ #category : #accessing }
SettingDeclaration >> hasValue [
	^ true
]

{ #category : #comparing }
SettingDeclaration >> hash [
	"hash is re-implemented because #= is re-implemented"
	^ self target hash
		bitXor: (self targetSelector hash
				bitXor: (self getSelector hash bitXor: self setSelector hash)) 
]

{ #category : #'user interface' }
SettingDeclaration >> index [
	^ self list
		indexOf: self realValue
		ifAbsent: [self realValue
				ifNil: [0]
				ifNotNil: [self addToList: self realValue.
					self list size]]
]

{ #category : #'user interface' }
SettingDeclaration >> index: anInteger [ 
	self realValue: (self list at: anInteger)
]

{ #category : #initialization }
SettingDeclaration >> initialize [

	super initialize.
	
	default := UniqueObject
]

{ #category : #'user interface' }
SettingDeclaration >> inputMorphFor: aContainer [
	^ (super inputMorphFor: aContainer)
		ifNotNil: [:im |			
				self whenPrivateChangedSend: #settingValueChanged: to: aContainer model.
				self whenPrivateChangedSend: #innerWidgetChanged to: aContainer.
				im]
]

{ #category : #'user interface' }
SettingDeclaration >> inputWidget [
	"return the default widget for the input a the setting"
	| inputWidget xtraDialogWidget | 
	xtraDialogWidget := super inputWidget.
	inputWidget :=  self localInputWidgetSelector ifNotNil: [:localSel | self perform: localSel].
	(inputWidget isNil and: [self typeClass notNil])
		ifTrue: [inputWidget := (self inputWidgetSelectorInClass: self typeClass)
			ifNotNil: [:selector | self typeClass perform: selector with: self]].
	inputWidget
		ifNotNil: [(inputWidget respondsTo: #model:) 
		  ifTrue: [inputWidget model ifNil: [inputWidget model: self]]].
	xtraDialogWidget 
		ifNotNil: [inputWidget := inputWidget ifNil: [xtraDialogWidget] 
			ifNotNil: [ self theme  newRowIn: World for: {inputWidget. xtraDialogWidget}]].
	^ inputWidget 
]

{ #category : #'user interface' }
SettingDeclaration >> inputWidgetForBoolean [
	^  self theme  
			newCheckboxIn: World
			for: self
			getSelected: #realValue
			setSelected: #realValue:
			getEnabled: #enabled
			label: '' 
			help: nil
]

{ #category : #'user interface' }
SettingDeclaration >> inputWidgetForColor [
	^ (  
	 self theme 
		newColorChooserIn: World
		for: self
		getColor: #realValue
		setColor: #realValue:
		getEnabled: #enabled 
		help: 'Choose a color') minWidth: 150; width: 150; yourself
]

{ #category : #'user interface' }
SettingDeclaration >> inputWidgetForDirectory [

	^ self inputWidgetForFileOrDirectoryWithAction: #chooseFileDirectory.
]

{ #category : #'user interface' }
SettingDeclaration >> inputWidgetForFileOrDirectoryWithAction: aSymbol [
	| list button |
	list := EditableDropListMorph
		on: self
		list: #list
		selected: #index
		changeSelected: #index:
		useIndex: true
		addToList: #addToList:
		class: String
		getEnabled: #enabled
		default: self defaultValue.
	list
		ghostText: self ghostHelp;
		hResizing: #rigid;
		width: 450;
		wantsFrameAdornments: true.
	button := self theme
		newButtonIn: World
		for: self
		getState: nil
		action: aSymbol
		arguments: nil
		getEnabled: nil
		label:
			(AlphaImageMorph new
				image: (self iconNamed: #smallOpenIcon))
		help: 'Open directory chooser dialog' translated.
	^ self theme
		newRowIn: World
		for:
			{list.
			button}
]

{ #category : #'user interface' }
SettingDeclaration >> inputWidgetForFont [
	^ (  self theme 
		newButtonIn: World
		for: self
		getState: nil 
		action: #getFont
		arguments: {}
		getEnabled: #enabled
		getLabel: #fontButtonLabel
		help: 'Choose a font ...' translated) cornerStyle: #square

]

{ #category : #'user interface' }
SettingDeclaration >> inputWidgetForLabel [
	| widget |
	widget := self theme newLabelIn: World for: self label: '' getEnabled: nil.
	widget getTextSelector: #preferenceValue.
	widget width: 450.
	^ widget
]

{ #category : #'user interface' }
SettingDeclaration >> inputWidgetForNumber [
	^ self inputWidgetForString width: 200.

]

{ #category : #'user interface' }
SettingDeclaration >> inputWidgetForPassword [
	| widget |
	widget :=  self theme  
				newTextEntryIn: World
				for: self
				get: #realValue
				set: #realValue:
				class: String
				getEnabled: #enabled
				help: nil.
	widget
		wantsFrameAdornments: true;
		hResizing: #rigid;
		width: 450; 
		font: (StrikeFont passwordFontSize:  self theme  textFont pointSize).
	^ widget
]

{ #category : #'user interface' }
SettingDeclaration >> inputWidgetForPoint [
	^ self inputWidgetForString width: 200.

]

{ #category : #'user interface' }
SettingDeclaration >> inputWidgetForString [
	| widget |
	widget := EditableDropListMorph
		on: self
		list: #list
		selected: #index
		changeSelected: #index:
		useIndex: true
		addToList: #addToList:
		class: (Smalltalk globals at: self type)
		getEnabled: #enabled
		default: self defaultValue.
	widget ghostText: self ghostHelp.
	widget contentMorph wantsFrameAdornments: true.
	widget hResizing: #rigid.
	widget width: 450.
	^ widget
]

{ #category : #'user interface' }
SettingDeclaration >> inputWidgetSelectorInClass: aClass [ 
	| selector |
	^ (aClass respondsTo: (selector := #settingInputWidgetForNode:))
		ifTrue: [selector] 
]

{ #category : #export }
SettingDeclaration >> isExportable [

	^ true
]

{ #category : #'user interface' }
SettingDeclaration >> list [
	^ self class valueListFor: self
]

{ #category : #'user interface' }
SettingDeclaration >> listenersForEvent: anEvent [
	| listeners actionSequence |	
	listeners := Set new.	
	actionSequence := self actionMap at: anEvent ifAbsent: [].	
	actionSequence ifNotNil: [
		actionSequence isCollection	 
			ifTrue: [actionSequence do: [:oneAction | 
					oneAction receiver ifNotNil: [:r | listeners add: r]]]
			ifFalse: [listeners add: actionSequence receiver]].
	^ listeners
]

{ #category : #'user interface' }
SettingDeclaration >> localInputWidgetSelector [
	| imSel |
	^ (self respondsTo: (imSel := ('inputWidgetFor' , self type) asSymbol)) ifTrue: [imSel]
]

{ #category : #'user interface' }
SettingDeclaration >> maxKept [
	"maximum number of old values which are kept in
	list instance variable"
	^ 5
]

{ #category : #'user interface' }
SettingDeclaration >> maxNameWidthForFont: aFont [ 
	| max |
	max := 0.
	self domainValuesLabels
		do: [:v | max := max max: (aFont widthOfStringOrText: v)].
	^ max 
]

{ #category : #'user interface' }
SettingDeclaration >> preferenceValue [
	^ self realValue
]

{ #category : #accessing }
SettingDeclaration >> privateChangeEvent [
	^ #privateSettingValueChanged
]

{ #category : #'user interface' }
SettingDeclaration >> privateForget: aReceiver [ 
	"do not notify aReceiver anymore"
	self removeActionsWithReceiver: aReceiver forEvent: self privateChangeEvent
]

{ #category : #'user interface' }
SettingDeclaration >> privateForget: aReceiver selector: aSelector [ 
	"Private"
	self
		removeActionsSatisfying: [:action | action receiver == aReceiver
				and: [action selector = aSelector]]
		forEvent: self privateChangeEvent
]

{ #category : #'user interface' }
SettingDeclaration >> privateListeners [
	^ self listenersForEvent: self privateChangeEvent
]

{ #category : #accessing }
SettingDeclaration >> realValue [
	"Answer the value of realValue"

	^ (self settingReceiver perform: self getSelector) 
		ifNil: [self setToDefault. 
			self settingReceiver perform: self getSelector]
]

{ #category : #accessing }
SettingDeclaration >> realValue: anObject [
	"Set the value of realValue"
	self settingReceiver perform: self setSelector with: anObject.
	self triggerEvent: self privateChangeEvent with: self.
]

{ #category : #accessing }
SettingDeclaration >> selector: aSymbol [
	self getSelector: aSymbol.
	self setSelector: (aSymbol, ':') asSymbol.
]

{ #category : #accessing }
SettingDeclaration >> setSelector [
	^ setSelector ifNil: [self getSelector ifNotNil: [:gs | (gs, ':') asSymbol]]
]

{ #category : #accessing }
SettingDeclaration >> setSelector: aSymbol [
	setSelector := aSymbol
]

{ #category : #'user interface' }
SettingDeclaration >> setToDefault [

	self realValue: self defaultValue.

]

{ #category : #'user interface' }
SettingDeclaration >> settingStoreOn: aStream [
	aStream tab;
		nextPutAll: (self target isSymbol
				ifTrue: [self target]
				ifFalse: [self target name]);
		nextPutAll: (self targetSelector
				ifNil: ['']
				ifNotNil: [' ' , self targetSelector]);
		 nextPutAll: ' ' , self setSelector , ' '.
	self preferenceValue settingStoreOn: aStream.
	aStream nextPut: $.;
		 cr
]

{ #category : #export }
SettingDeclaration >> startupAction [
	"I assume here that the receiver is always a class"

	 | targetSymbol currentValue |
			
	targetSymbol := self target isSymbol
						ifTrue: [ self target ]
						ifFalse: [ self target name asSymbol ].
						
	currentValue := self targetSelector
						ifNil: [ (Smalltalk at: targetSymbol) perform: self getSelector ]
						ifNotNil: [:s | ((Smalltalk at: targetSymbol) perform: s) perform: self getSelector].
	^ StartupAction 
		name: self label 
		code: (String streamContents: [:s |
					self targetSelector ifNotNil: [ s << '(' ].
					s << '(Smalltalk at: '.
					targetSymbol asSymbol printOn: s.
					self targetSelector ifNotNil: [:ts | s << ') perform: '. ts printOn: s ].
					s << ') perform: '.
					self setSelector asSymbol printOn: s.
					s << ' with: ('.
					currentValue settingStoreOn: s.
					s <<')' ])
		runOnce: true
]

{ #category : #accessing }
SettingDeclaration >> type [
	"Answer the value of type"

	^ type ifNil: [type := self realValue class name]
]

{ #category : #accessing }
SettingDeclaration >> type: anObject [
	"Set the value of type"

	type := anObject
]

{ #category : #accessing }
SettingDeclaration >> typeClass [
	^ Smalltalk globals at: self type ifAbsent: [  ]
]

{ #category : #'user interface' }
SettingDeclaration >> whenPrivateChangedSend: zeroOrOneArgSelector to: aSubscriber [ 
	"private, use #whenChangedSend:to: instead"
	self
		when: self privateChangeEvent
		send: zeroOrOneArgSelector
		to: aSubscriber
]
