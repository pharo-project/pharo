running
initializeBootstrapEnvironment
	
	bootstrapEnvironment := Dictionary new.
	
	"We initialize the object space with a boot symbol table and character table.
	We will later replace them by real ones."
	objectSpace := EPObjectSpace new.
	objectSpace backend: EPOzVMBackend new.
	objectSpace characterTable: (EPExternalCharacterTable new objectSpace: objectSpace; yourself).
	objectSpace symbolTable: (EPExternalSymbolTable new objectSpace: objectSpace; yourself).

	classLoader createJustStubs.
	classLoader environment: bootstrapEnvironment.
	classLoader systemDefinition: systemDefinition.
	classLoader builder: self.
	
	bootstrapInterpreter := EPASTEvaluator new.
	bootstrapInterpreter codeProvider: self.
	bootstrapInterpreter environment: self.
	bootstrapInterpreter objectSpace: objectSpace.
	objectSpace interpreter: bootstrapInterpreter.