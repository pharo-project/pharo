initialization
setActionResetButtom
	
	^ [(UIManager default confirm: 'Do you want to delete the current configuration
 and create a new configuration ?')
		ifTrue: [ self  delete. 
				SelectPackageBrowser open.] ]