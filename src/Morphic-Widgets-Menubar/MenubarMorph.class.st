Class {
	#name : #MenubarMorph,
	#superclass : #DockingBarMorph,
	#instVars : [
		'menuBarItems'
	],
	#category : #'Morphic-Widgets-Menubar'
}

{ #category : #'instance creation' }
MenubarMorph class >> closeAll [

	World submorphs
		select: [ :e | e isKindOf: MenubarMorph ]
		thenDo: [ :e | e delete ].
]

{ #category : #'instance creation' }
MenubarMorph class >> initialize [

	self install.
	self reset.
]

{ #category : #'instance creation' }
MenubarMorph class >> install [
	"Unsubscribe to subscribe only once"
	SystemAnnouncer uniqueInstance unsubscribe: self.
	SystemAnnouncer uniqueInstance weak
		when: MethodAnnouncement
		send: #methodAnnouncementReceived: to: self.
]

{ #category : #'instance creation' }
MenubarMorph class >> methodAnnouncementReceived: anAnnouncement [

	(anAnnouncement methodAffected hasPragmaNamed: #worldMenu)
		ifTrue: [ self reset ]
]

{ #category : #'instance creation' }
MenubarMorph class >> open [
	"self open"

	self new
		menuBarItems: WorldState new menuBuilder menuSpec items;
		open.
]

{ #category : #'instance creation' }
MenubarMorph class >> reset [

	<script>
	self closeAll.
	self open.
]

{ #category : #construction }
MenubarMorph >> addSeparator [
	self addMorphBack: MenubarSeparatorMorph new
]

{ #category : #adding }
MenubarMorph >> drawSubmenuMarkerOn: aCanvas [

	"Nothing"
]

{ #category : #initialization }
MenubarMorph >> initialize [

	super initialize.
	self autoGradient: false.
	self cellInset: 7.
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

	self menuBarItems do: [ :each |
		self
			add: each label
			icon: each icon
			help: each help
			subMenu: (each subMenu ifNotNil: #asMenuMorph).
			each separator ifTrue: [ self addSeparator ] ].
		
	self
		adhereToTop;
		openInWorld
]

{ #category : #accessing }
MenubarMorph >> repelsMorph: aMorph event: ev [
	^ true
]

{ #category : #accessing }
MenubarMorph >> wantsYellowButtonMenu [
	"Answer true if the receiver wants a yellow button menu."
	
	^false
]
