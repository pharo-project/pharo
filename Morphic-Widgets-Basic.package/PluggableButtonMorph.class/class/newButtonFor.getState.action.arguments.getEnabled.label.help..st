instance creation
newButtonFor: aModel 
	getState: stateSel 
	action: actionSel 
	arguments: args 
	getEnabled: enabledSel 
	label: label 
	help: helpText
	"Answer a new button."

	| b |
	b := self on: aModel getState: stateSel action: actionSel.
	b
		arguments: (args ifNil: [{b}]);
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		label: label ;
		getEnabledSelector: enabledSel;
		setBalloonText: helpText;
		extent: b minExtent;
		hResizing: #rigid;
		vResizing: #rigid.
	^b