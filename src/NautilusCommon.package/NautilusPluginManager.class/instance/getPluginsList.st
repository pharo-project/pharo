lists behavior
getPluginsList

	^ pluginsList ifNil: [ pluginsList := Nautilus pluginClasses copy ].