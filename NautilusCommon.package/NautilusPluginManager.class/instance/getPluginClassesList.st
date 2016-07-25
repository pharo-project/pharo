lists behavior
getPluginClassesList

	^ AbstractNautilusPlugin allSubclasses sort: [:a :b | a pluginName <= b pluginName ]