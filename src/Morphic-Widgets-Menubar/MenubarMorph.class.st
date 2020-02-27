"
Description
--------------------

I am a Morph of the form of a Menubar that can be displayed at the top of the World. 

You can disable me with: `self showMenubar: false`.

For now, when you open me, by default I will use the world menu pragma to collect the items to display. If you want a custom menu, just update the WorldMenu pragma to use.
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	menuBarItems:		<aCollection>	A collection of items to show at the root of the Menubar. Those items can also contains submenus.

"
Class {
	#name : #MenubarMorph,
	#superclass : #DockingBarMorph,
	#instVars : [
		'menuBarItems'
	],
	#classVars : [
		'ShowMenubar'
	],
	#category : #'Morphic-Widgets-Menubar-View'
}

{ #category : #'instance creation' }
MenubarMorph class >> closeAll [

	self currentWorld submorphs
		select: [ :e | e isKindOf: MenubarMorph ]
		thenDo: [ :e | e delete ].
]

{ #category : #'instance creation' }
MenubarMorph class >> initialize [
	self showMenubar: true.
	self install.
	self reset
]

{ #category : #'instance creation' }
MenubarMorph class >> install [
	"Unsubscribe to subscribe only once"
	SystemAnnouncer uniqueInstance unsubscribe: self.
	SystemAnnouncer uniqueInstance weak
		when: MethodAnnouncement
		send: #methodAnnouncementReceived: to: self.
]

{ #category : #setting }
MenubarMorph class >> menubarSettingOn: aBuilder [
	<systemsettings>
	(aBuilder setting: #showMenubar)
		target: self;
		parent: #morphic;
		label: 'Show menubar';
		default: true;
		description: 'Whether the world''s menubar should be shown or not.'
]

{ #category : #'instance creation' }
MenubarMorph class >> methodAnnouncementReceived: anAnnouncement [

	(anAnnouncement methodAffected hasPragmaNamed: #worldMenu)
		ifTrue: [ self reset ]
]

{ #category : #'instance creation' }
MenubarMorph class >> open [
	<script>
	
	self showMenubar ifFalse: [ ^ self ].

	self new
		menuBarItems: WorldState new menuBuilder menuSpec items;
		open.
]

{ #category : #initialization }
MenubarMorph class >> reset [

	<script>
	self closeAll.
	self open.
]

{ #category : #accessing }
MenubarMorph class >> showMenubar [
	^ ShowMenubar
]

{ #category : #accessing }
MenubarMorph class >> showMenubar: aBoolean [
	ShowMenubar := aBoolean.
	self reset
]

{ #category : #construction }
MenubarMorph >> addSeparator [
	self addMorphBack: MenubarSeparatorMorph new
]

{ #category : #initialization }
MenubarMorph >> defaultBorderWidth [
	^ self theme menubarBorderWidthFor: self
]

{ #category : #initialization }
MenubarMorph >> defaultColor [
	^ self theme menubarFillStyleFor: self
]

{ #category : #adding }
MenubarMorph >> drawSubmenuMarkerOn: aCanvas [

	"Nothing"
]

{ #category : #initialization }
MenubarMorph >> initialize [
	super initialize.
	self cellInset: 7
]

{ #category : #testing }
MenubarMorph >> isMenubar [
	^ true
]

{ #category : #accessing }
MenubarMorph >> menuBarItems [
	^ menuBarItems
]

{ #category : #accessing }
MenubarMorph >> menuBarItems: anObject [
	menuBarItems := anObject
]

{ #category : #construction }
MenubarMorph >> newMenuItem [
	
	^ MenubarItemMorph new
]

{ #category : #accessing }
MenubarMorph >> open [
	self menuBarItems
		do: [ :each | 
			self
				add: each label
				icon: each icon
				help: each help
				subMenu: (each subMenu ifNotNil: #asMenubarMenuMorph)
				action: each action
				keyText: each keyText.
			each separator
				ifTrue: [ self addSeparator ] ].
	self
		adhereToTop;
		openInWorld
]

{ #category : #recategorized }
MenubarMorph >> rejectsEvent: anEvent [
	(anEvent isMouse and: [ anEvent isMouseDown ]) ifTrue: [ ^ (self submorphs anySatisfy: [ :each | each containsPoint: anEvent cursorPoint ]) not ].
	
	^ super rejectsEvent: anEvent
]

{ #category : #accessing }
MenubarMorph >> repelsMorph: aMorph event: ev [
	^ true
]

{ #category : #initialization }
MenubarMorph >> wantsGradientByDefault [
	^ false
]

{ #category : #accessing }
MenubarMorph >> wantsYellowButtonMenu [
	"Answer true if the receiver wants a yellow button menu."
	
	^false
]
