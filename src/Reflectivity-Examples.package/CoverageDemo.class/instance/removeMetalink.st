action
removeMetalink
	
	compiledMethods do:[:method| method methodClass recompile].
	