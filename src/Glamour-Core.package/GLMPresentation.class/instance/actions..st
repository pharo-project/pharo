scripting actions
actions: aBlock 
	self deprecated: 'use dynamicActionsOnSelection: for contextual actions, and dynamicActions for presentation-wide actions'.
	^ self dynamicActionsOnSelection: aBlock 