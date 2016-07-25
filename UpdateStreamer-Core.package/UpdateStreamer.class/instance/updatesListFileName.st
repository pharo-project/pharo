accessing
updatesListFileName
	"Returns the name of the update file"
	
	^ updatesListFileName ifNil: [ updatesListFileName := self defaultUpdatesListFileName ]