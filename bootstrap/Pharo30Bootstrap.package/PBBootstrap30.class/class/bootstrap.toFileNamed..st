bootstrapping
bootstrap: version toFileNamed: aFileName

	| definition bootstrap edit |
	definition := (FiMetacelloWriter new written: (ConfigurationOfPharoBootstrap project version: version)).

	edit := definition ficusPackages 
		editAtPath: FiPath root / #Slot / #contents / #SlotClassBuilder / #methods
		do: [ :elem | elem editInclude: (FiMethod sourceCode: 'validateClassName') ].
	definition := (definition ficusPackages resultOf: edit) asRFiEnvironment.
	
	edit := definition ficusPackages
		editAtPath: FiPath root / #Traits / #contents / #TClass / #methods
		do: [ :elem | elem editInclude: (FiMethod sourceCode: 'setName: aSymbol andRegisterInCategory: categorySymbol environment: aSystemDictionary	
	(self environment == aSystemDictionary
		and: [self name = aSymbol
			and: [self category = categorySymbol]]) ifTrue: [^self].

	(self environment notNil and: [self name notNil and: [self name ~= aSymbol]]) ifTrue: [
		self environment renameClass: self as: aSymbol].
	
	self name: aSymbol.
	self environment: aSystemDictionary.	
	self environment at: self name put: self.
	self environment organization classify: self name under: categorySymbol.
	^ true') ].
	definition := (definition ficusPackages resultOf: edit) asRFiEnvironment.

	edit := definition ficusPackages
		editAtPath: FiPath root / #Traits / #contents / #TBehavior / #methods
		do: [ :elem | elem editInclude: (FiMethod sourceCode: 'addTraitSelector: aSymbol withMethod: aCompiledMethod
	| source method |
	method := aCompiledMethod copy.
	method selector: aSymbol.
	method methodClass: self.
	self isTrait ifFalse: [ method properties at: #traitSource put: aCompiledMethod ].
	self basicAddSelector: aSymbol withMethod: method') ].
	definition := (definition ficusPackages resultOf: edit) asRFiEnvironment.

	bootstrap := self basicNew
		initializeSystemDefinition: definition;
		initializeBootstrapEnvironment;
		createVMStubs;
		createInitialObjects;
		createClasses;
		installMethods;
		initializeImage.
		
	bootstrap objectSpace serializeInFileNamed: aFileName