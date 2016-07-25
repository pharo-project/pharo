widget API
subMenu
	| subMenu | 
	
	subMenu := self model subMenu.
	subMenu ifNil: [ ^ nil ].
	
	^ (subMenu isBlock or: [ subMenu isMessageSend ])
		ifTrue: [ subMenu cull: self model ]
		ifFalse: [ subMenu buildWithSpecAsPopup ]