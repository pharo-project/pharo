actions
postload: loader package: packageSpec

	| initializersEnabled |

	Author fullName: self class name.
	
	initializersEnabled := MCMethodDefinition initializersEnabled.

	MCMethodDefinition initializersEnabled: false.
	Gofer it smalltalkhubUser: 'Pharo' project: 'Pharo60'; package: 'Compiler';  load.
	Gofer it smalltalkhubUser: 'Pharo' project: 'Pharo60'; package: 'RPackage-Tests';  load.	
	MCMethodDefinition initializersEnabled: true.

	(Smalltalk at: #ParseNode) initialize.
	(Smalltalk at: #Scanner) initialize.
	(Smalltalk at: #Decompiler) initialize.
	(Smalltalk at: #MessageNode) initialize.
	(Smalltalk at: #VariableNode) initialize.
	
	Smalltalk tools register: GTGenericStackDebugger as: #debugger.	

	Smalltalk cleanOutUndeclared. 
	Smalltalk fixObsoleteReferences.
	
	EpLostChangesDetector class compile: 'startUp: isImageStarting'.
	
	Author reset.

	MCMethodDefinition initializersEnabled: initializersEnabled.
	
	self additionalInitialization.