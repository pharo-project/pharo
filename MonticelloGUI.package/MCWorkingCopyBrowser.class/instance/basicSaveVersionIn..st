actions
basicSaveVersionIn: aRepository
	| newVersion waitForVersion |
	waitForVersion := Semaphore new.
	
	UIManager default defer: [
		newVersion := workingCopy newVersionIn: aRepository.
		waitForVersion signal ].
	
	Processor activeProcess == UIManager default uiProcess
		ifFalse: [ waitForVersion wait ].
	newVersion ifNil: [ ^ self ]. 
	
	Cursor wait showWhile: [[ 
		self
			storeVersion: newVersion in: aRepository;
			storeDependencies: newVersion in: aRepository.] 
			ensure: [ (MCVersionInspector new version: newVersion) show ]]