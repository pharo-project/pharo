panes
updatablePanes
	"Answer the list of panes, in order, which should be sent the #verifyContents message"
	^ updatablePanes ifNil: [updatablePanes := #()]