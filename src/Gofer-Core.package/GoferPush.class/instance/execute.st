running
execute
	self model 
		do: [ :assocation | assocation value storeVersion: assocation key version ]
		displayingProgress: 'Pushing Versions'