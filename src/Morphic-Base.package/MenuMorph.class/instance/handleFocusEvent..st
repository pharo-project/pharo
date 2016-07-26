events
handleFocusEvent: evt
	"Handle focus events. Valid menu transitions are determined based on the menu currently holding the focus after the mouse went down on one of its children."
	self processEvent: evt.

	"Need to handle keyboard input if we have the focus."
	evt isKeyboard ifTrue: [^ self handleEvent: evt].

	"We need to handle button clicks outside and transitions to local popUps so throw away everything else"
	(evt isMouseOver or:[evt isMouse not]) ifTrue:[^self].
	"What remains are mouse buttons and moves"
	evt isMove ifFalse:[^self handleEvent: evt]. "handle clicks outside by regular means"
	"Now it's getting tricky. On #mouseMove we might transfer control to *either* the currently active submenu or the pop up owner, if any. Since the active sub menu is always displayed upfront check it first."
	selectedItem ifNotNil:[(selectedItem activateSubmenu: evt) ifTrue:[^self]].
	"If we aren't under the hand anymore, transfer to the owner"
   (self fullContainsPoint: evt position) ifFalse:[
	popUpOwner ifNotNil:[(popUpOwner activateOwnerMenu: evt) ifTrue:[^self]]]