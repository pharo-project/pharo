building widgets
buildNotifierPaneWidgetsSpec
	
	^ self message 
		ifNil: [ {#stackPane. #MultiColumnListPresenter} ] 
		ifNotNil: [ {#codePane. #TextPresenter} ]