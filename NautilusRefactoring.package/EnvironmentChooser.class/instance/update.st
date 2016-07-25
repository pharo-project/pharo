updating
update
	self environmentsView
		emptyList.
		
	self enableGlobal ifTrue: [ 
		self environmentsView addItemLabeled: 'System-wide' do: [ 
			self environment: RBBrowserEnvironment new ]].
	
	self environments do: [ :environemnt| |capturedValue|
		capturedValue := environemnt.
		self environmentsView
			addItemLabeled: capturedValue label
			do: [ self environment: capturedValue ]].
	
	self enableCustom ifTrue: [ 
		self environmentsView addItemLabeled: 'Custom...' do: [
			self chooseNewEnvironment ifNotNil: [ :newEnvironment | 
				self environment: newEnvironment ]]].