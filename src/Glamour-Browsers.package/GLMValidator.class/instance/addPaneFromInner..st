private
addPaneFromInner: aPort 
	| newTransmission realTransmissions lastActivePresentation |
	"the lastActivePresentation is needed for knowing what presentation to show by default"
	self panes isEmpty ifFalse: [lastActivePresentation := self panes last lastActivePresentation ].
	self panes addLast: ( (GLMPane in: self) lastActivePresentation: lastActivePresentation; yourself).
	self newTransmission
		addActiveOrigin: aPort; 
		destination: (self panes last port: #entity); 
		presentations: (self presentations copy); 
		transmit. 
	realTransmissions := self transmissions collect: [:each |  
			newTransmission := GLMTransmission new ensureReplacePresentationsStrategy. 
			each activeOrigins do: [:eachPort | 
				newTransmission addActiveOrigin: (aPort pane port: eachPort name)]. 
			each passiveOrigins do: [:eachPort | 
				newTransmission addPassiveOrigin: (aPort pane port: eachPort name)]. 
			newTransmission 
				destination: (self panes last port: #entity); 
				addPresentations: (self presentations collect: #copy);
				addPresentations: (each presentations collect: #copy)] . 
	(realTransmissions select: [:each | each originatesAt: aPort]) 
		do: [:each | each transmit]. 