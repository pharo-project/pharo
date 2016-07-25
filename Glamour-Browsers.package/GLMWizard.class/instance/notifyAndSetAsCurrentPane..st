building pane
notifyAndSetAsCurrentPane: aGLMPane

	|oldPane   tmpPanes|
	tmpPanes := self stepsAndPanes at: self currentStep ifAbsentPut: [OrderedCollection new].
	tmpPanes removeLast.
	tmpPanes addLast: aGLMPane.
	"self panes addLast: aGLMPane. " 
	self announce: (GLMPaneReplaced 
							oldPane: oldPane 
							newPane: tmpPanes last
							fromBrowser: self ) 
	