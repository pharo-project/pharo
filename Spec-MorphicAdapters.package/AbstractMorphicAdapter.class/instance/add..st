protocol
add: aWidget
	
	self widgetDo: [ :w | w ensureLayoutAndAddMorph: aWidget asWidget ]