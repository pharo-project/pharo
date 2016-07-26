installing
isInstalled

	^ Nautilus pluginClasses anySatisfy: [ :pluginBinding | 
			pluginBinding first = self ]