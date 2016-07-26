styling
removeIconStyle
	|conf|
	
	conf := RubConfigurationChange new.
	conf configurationBlock: [:text | 
		text segments copy do: #delete].  
	self sourceTextModel announce: conf.