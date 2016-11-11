actions
postload: loader package: packageSpec

	| gofer |
	
	Nautilus registerToolsOn: Smalltalk tools.
	Smalltalk tools register: TestRunner as: #testRunner.
	Smalltalk tools register: MCWorkingCopyBrowser as: #monticelloBrowser.
	
	"reload icons because of new tools" 
 	gofer := Gofer it smalltalkhubUser: 'Pharo' project: 'Pharo60'.
   gofer package: #'Polymorph-TaskbarIcons'.
   gofer load.	

	BIConfigurableFormatter initialize.
	
	Smalltalk garbageCollect.
	Smalltalk cleanOutUndeclared. 

	Author reset.
