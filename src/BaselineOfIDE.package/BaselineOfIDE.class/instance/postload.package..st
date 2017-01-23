actions
postload: loader package: packageSpec

	| initializersEnabled repo gofer |

	Author fullName: self class name.
	
	initializersEnabled := MCMethodDefinition initializersEnabled.

	MCMethodDefinition initializersEnabled: false.
	
	repo := MCFileTreeRepository new
	directory: './pharo-core/src' asFileReference;
	yourself.
		
	gofer := Gofer it repository: repo.
	gofer package: #'Compiler'.
	gofer package: #'RPackage-Tests'.
	gofer load.
	
	MCMethodDefinition initializersEnabled: initializersEnabled.

	(Smalltalk at: #ParseNode) initialize.
	(Smalltalk at: #Scanner) initialize.
	(Smalltalk at: #Decompiler) initialize.
	(Smalltalk at: #MessageNode) initialize.
	(Smalltalk at: #VariableNode) initialize.
	
	Smalltalk tools register: GTGenericStackDebugger as: #debugger.	

	Smalltalk cleanOutUndeclared. 
	Smalltalk fixObsoleteReferences.
	
	self class environment at: #EpLostChangesDetector ifPresent: [:aClass |
	  aClass class compile: 'startUp: isImageStarting'. ].
	
	Author reset.

	MCMethodDefinition initializersEnabled: initializersEnabled.
	
	self additionalInitialization.