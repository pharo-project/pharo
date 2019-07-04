Class {
	#name : #MorphicMenuBarAdapter,
	#superclass : #MorphicBaseMenuAdapter,
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #private }
MorphicMenuBarAdapter >> adoptMenuGroupModel: aGroupModel first: aBoolean [
	| controls |
	
	aBoolean ifFalse: [ 
		widget 
			addMorphBack: (self theme newToolSpacerIn: widget);
			addMorphBack: (self theme newToolbarHandleIn: widget);
			addMorphBack: (self theme newToolSpacerIn: widget) ].
			
	controls := OrderedCollection new.
	aGroupModel menuItems do: [ :item | 
		self adoptMenuItemModel: item accumulator: controls ].
	
	self emptyAccumulator: controls
]

{ #category : #private }
MorphicMenuBarAdapter >> adoptMenuItemModel: item accumulator: controls [
	
	item subMenu
		ifNil: [ | button |
			button := self buildButtonFor: item.
			controls add: button ]
		ifNotNil: [ 
			self emptyAccumulator: controls.
			widget addMorphBack: (self theme newToolSpacerIn: widget).
			widget 
				add: item name
				font: self theme menuBarFont
				icon: item icon
				help: item description
				subMenu: item subMenu buildWithSpecAsPopup.
			widget addMorphBack: (self theme newToolSpacerIn: widget) ]
]

{ #category : #private }
MorphicMenuBarAdapter >> buildButtonFor: item [

	^ self theme
		newButtonIn: widget
		for: item
		getState: nil
		action: #performMenuActionWith:
		arguments: {widget}
		getEnabled: #enabled
		label: (IconicListItem text: item name icon: item icon)
		help: item description
]

{ #category : #factory }
MorphicMenuBarAdapter >> buildWidget [
	widget := ToolDockingBarMorph new
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		yourself.

	self menuGroups doWithIndex: [ :aGroup :index | 
		self adoptMenuGroupModel: aGroup first: index = 1 ].

	^ widget
]

{ #category : #private }
MorphicMenuBarAdapter >> emptyAccumulator: controls [

	controls do: [ :button | widget addMorphBack: button ].
	controls removeAll
]

{ #category : #accessing }
MorphicMenuBarAdapter >> menuGroup [
	^ self presenter menuGroup
]
