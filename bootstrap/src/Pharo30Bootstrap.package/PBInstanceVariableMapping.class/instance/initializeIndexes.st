initialization
initializeIndexes
	contextPcIndex := self lookup: #pc in: #Context.
	contextReceiverIndex := self lookup: #receiver in: #Context.
	processContextIndex := self lookup: #suspendedContext in: #Process.
	classMethodDictionaryIndex := self lookup: #methodDict in: #Class.
	classSuperclassIndex := self lookup: #superclass in: #Class.
	processNameIndex := self lookup: #name in: #Process.
	traitClassTraitIndex := self lookup: #classTrait in: #Trait.
	traitMethodDictionaryIndex := self lookup: #methodDict in: #Trait.
	contextSenderIndex := self lookup: #sender in: #Context.
	classPoolIndex := self lookup: #classPool in: #Class.
	contextStackpIndex := self lookup: #stackp in: #Context.
	contextMethodIndex := self lookup: #method in: #Context.
	classNameIndex := self lookup: #name in: #Class.
	processSchedulerActiveProcessIndex := self lookup: #activeProcess in: #ProcessorScheduler.
	processPriorityIndex := self lookup: #priority in: #Process.
	contextClosureOrNilIndex := self lookup: #closureOrNil in: #Context.
	classFormatIndex := self lookup: #format in: #Class.
	processPriorityListIndex := self lookup: #myList in: #Process.
	processNextLinkIndex := self lookup: #nextLink in: #Process.
	traitNameIndex := self lookup: #name in: #Trait.

	smalltalkImageGlobalsIndex := self lookup: #globals in: #SmalltalkImage.
	classSharedPoolsIndex := self lookup: #sharedPools in: #Class.
	classNumberOfVariables := self numberOfInstanceVariablesOf: #Class.
	classInstanceVariableNamesIndex := self lookup: #instanceVariableNames in: #Class.
	dictionaryArrayIndex := self lookup: #array in: #Dictionary.
	
	listOfClassSlots := self allSlotsOf: #Class.