initialize
client: aMorph click: aClickSelector dblClick: aDblClickSelector dblClickTime: timeOut dblClickTimeout: aDblClickTimeoutSelector drag: aDragSelector threshold: aNumber event: firstClickEvent
	clickClient := aMorph.
	clickSelector := aClickSelector.
	dblClickSelector := aDblClickSelector.
	dblClickTime := timeOut.
	dblClickTimeoutSelector := aDblClickTimeoutSelector.
	dragSelector := aDragSelector.
	dragThreshold := aNumber.
	firstClickDown := firstClickEvent.
	firstClickTime := firstClickEvent timeStamp.
	clickState := #firstClickDown.