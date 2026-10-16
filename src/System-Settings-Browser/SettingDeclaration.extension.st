Extension { #name : 'SettingDeclaration' }

{ #category : '*System-Settings-Browser' }
SettingDeclaration >> hasEditableList [
	^ (self realValue isKindOf: Boolean) not
]

{ #category : '*System-Settings-Browser' }
SettingDeclaration >> inputMorphFor: aContainer [
	^ (super inputMorphFor: aContainer)
		ifNotNil: [:im |
				self whenPrivateChangedSend: #settingValueChanged: to: aContainer model.
				self whenPrivateChangedSend: #innerWidgetChanged to: aContainer.
				im]
]

{ #category : '*System-Settings-Browser' }
SettingDeclaration >> inputWidget [
	"return the default widget for the input a the setting"
	| inputWidget xtraDialogWidget |
	xtraDialogWidget := super inputWidget.
	inputWidget :=  self localInputWidgetSelector ifNotNil: [:localSel | self perform: localSel].
	
	(inputWidget isNil and: [self typeClass isNotNil])
		ifTrue: [ inputWidget := self typeClass settingInputWidgetForNode: self ].

	(inputWidget notNil and: [ inputWidget respondsTo: #model: ])
		ifTrue: [ inputWidget model ifNil: [ inputWidget model: self ] ].

	xtraDialogWidget
		ifNotNil: [inputWidget := inputWidget ifNil: [xtraDialogWidget]
			ifNotNil: [ self theme  newRowIn: self for: {inputWidget. xtraDialogWidget}]].
	^ inputWidget
]

{ #category : '*System-Settings-Browser' }
SettingDeclaration >> inputWidgetForBoolean [
	^  self theme
			newCheckboxIn: self
			for: self
			getSelected: #realValue
			setSelected: #realValue:
			getEnabled: #enabled
			label: ''
			help: nil
]

{ #category : '*System-Settings-Browser' }
SettingDeclaration >> inputWidgetForColor [
	^ (
	 self theme
		newColorChooserIn: self
		for: self
		getColor: #realValue
		setColor: #realValue:
		getEnabled: #enabled
		help: 'Choose a color') minWidth: 150; width: 150; yourself
]

{ #category : '*System-Settings-Browser' }
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
		newButtonIn: self
		for: self
		getState: nil
		action: aSymbol
		arguments: nil
		getEnabled: nil
		label:
			(AlphaImageMorph new
				image: (self iconNamed: #smallOpen))
		help: 'Open directory chooser dialog'.
	^ self theme
		newRowIn: self
		for:
			{list.
			button}
]

{ #category : '*System-Settings-Browser' }
SettingDeclaration >> inputWidgetForFont [
	^ (self theme
		newButtonIn: self
		for: self
		getState: nil
		action: #getFont
		arguments: {}
		getEnabled: #enabled
		getLabel: #fontButtonLabel
		help: 'Choose a font ...') cornerStyle: #square
]

{ #category : '*System-Settings-Browser' }
SettingDeclaration >> inputWidgetForLabel [
	| widget |
	widget := self theme newLabelIn: self for: self label: '' getEnabled: nil.
	widget getTextSelector: #preferenceValue.
	widget width: 450.
	^ widget
]

{ #category : '*System-Settings-Browser' }
SettingDeclaration >> inputWidgetForNumber [
	^ self inputWidgetForString width: 200
]

{ #category : '*System-Settings-Browser' }
SettingDeclaration >> inputWidgetForPassword [
	| widget |
	widget :=  self theme
				newTextEntryIn: self
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

{ #category : '*System-Settings-Browser' }
SettingDeclaration >> inputWidgetForPoint [
	^ self inputWidgetForString width: 200
]

{ #category : '*System-Settings-Browser' }
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
	self ghostHelp ifNotNil: [ :gh | widget ghostText: gh ].
	widget contentMorph wantsFrameAdornments: true.
	widget hResizing: #rigid.
	widget width: 450.
	^ widget
]

{ #category : '*System-Settings-Browser' }
SettingDeclaration >> isExportable [

	^ true
]

{ #category : '*System-Settings-Browser' }
SettingDeclaration >> localInputWidgetSelector [
	| imSel |
	^ (self respondsTo: (imSel := ('inputWidgetFor' , self type) asSymbol)) ifTrue: [imSel]
]
