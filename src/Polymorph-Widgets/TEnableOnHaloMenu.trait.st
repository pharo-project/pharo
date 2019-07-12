"
Trait providing an ""enabled"" option on the halo menu.
"
Trait {
	#name : #TEnableOnHaloMenu,
	#category : #'Polymorph-Widgets-Traits'
}

{ #category : #'menu & halo' }
TEnableOnHaloMenu >> addToggleItemsToHaloMenu: aCustomMenu [ 
	"Add toggle-items to the halo menu"
	
	super addToggleItemsToHaloMenu: aCustomMenu.
	aCustomMenu
		addUpdating: #enabledString
		target: self
		selector: #toggleEnabled
]

{ #category : #accessing }
TEnableOnHaloMenu >> enabled [
	"Answer the enabled state of the receiver."

	self explicitRequirement
]

{ #category : #accessing }
TEnableOnHaloMenu >> enabled: aBoolean [
	"Set the enabled state of the receiver."

	self explicitRequirement
]

{ #category : #enablement }
TEnableOnHaloMenu >> enabledString [
	"Answer the string to be shown in a menu to represent the 'enabled' status"
	
	^ (self enabled) -> 'enabled' translated
]

{ #category : #enablement }
TEnableOnHaloMenu >> toggleEnabled [
	"Toggle the enabled state."

	self enabled: self enabled not
]
