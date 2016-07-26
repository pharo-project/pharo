private
addPaneFromInner: aPort 
	"This method adds a new pane at the end of the finder. Data comes from a port that is in a pane inside the current browser."
	| newPane lastActive |
	"the lastActivePresentation is needed for knowing what presentation to show by default"
	lastActive := self panes isEmpty ifTrue: [nil] ifFalse: [ self lastActivePresentation ].
	newPane := GLMPane named: (self panes size + 1) asString in: self.
	newPane lastActivePresentation: lastActive.
	self panes addLast: newPane.
	self newTransmission
		addActiveOrigin: aPort; 
		destination: (newPane port: #entity); 
"		presentations: (self presentations copy); "
		transmit. 


	"This code was supposed to offer custom transmissions"
"	realTransmissions := self transmissions collect: [:oldTransmission |  
			newTransmission := GLMTransmission new ensureReplacePresentationsStrategy. 
			oldTransmission activeOrigins do: [:eachPort | 
				newTransmission addActiveOrigin: (aPort pane port: eachPort name)]. 
			oldTransmission passiveOrigins do: [:eachPort | 
				newTransmission addPassiveOrigin: (aPort pane port: eachPort name)]. 
			newTransmission 
				destination: (newPane port: #entity); 
				addPresentations: (self presentations collect: #copy);
				addPresentations: (oldTransmission presentations collect: #copy)] . 
	[realTransmissions isEmpty] assert.
	(realTransmissions select: [:each | each originatesAt: aPort]) 
		do: [:each | each transmit]. "