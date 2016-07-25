private
doDebug: evt with: menuHandle
	"Ask hand to invoke the a debugging menu for my inner target.  If shift key is down, immediately put up an inspector on the inner target"

	| menu |
	"self obtainHaloForEvent: evt andRemoveAllHandlesBut: nil."
	self world displayWorld.
	evt shiftPressed ifTrue: 
		[self delete.
		^ innerTarget inspectInMorphic: evt].

	menu := innerTarget buildDebugMenu: evt hand.
	menu buildTitle: [ :menuTitle | menuTitle
		title: (innerTarget externalName truncateWithElipsisTo: 40);
		"icon: Smalltalk ui icons smallDebugIcon;"
		withCloseBox;
		withPinBox
	];
	popUpEvent: evt in: self world