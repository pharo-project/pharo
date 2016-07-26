copying
postCopy
	| newPane newPanes newPanesMapping newTransmission oldPane oldWatcherPane |
	super postCopy.
	"needed for setting the right panes for the new transmissions"
	newPanesMapping := Dictionary new. 
	newPanes := OrderedCollection new.
	self panes do: [:each | 
		newPane := each copy.
		newPane browser: self.
		newPanes addLast: newPane.
		newPanesMapping at: each put: newPane ].
	oldPane := self pane.
	oldWatcherPane := self watcherPane.
	watcherPane := nil.
	pane := nil.
	newPanesMapping at: oldPane ifAbsentPut: [ self pane ].
	newPanesMapping at: oldWatcherPane ifAbsentPut: [ self watcherPane ].
	self flag: 'we should also copy the values of ports from pane and watcherPane'.
	transmissions := self transmissions collect: [ :each | 
		newTransmission := each copy.
		newTransmission 
			changePortsAccordingToPaneMapping: newPanesMapping 
			fromOldBrowser: nil
			toNewBrowser: self.
		newTransmission ].
	panes := newPanes