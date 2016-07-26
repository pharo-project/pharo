I handle .fuel files that are passed as arguments when starting the image. 

Usage: fuel [save] [quit] <fuelFile>
 
	save    save the image after loading <fuelFile>
	quit    Don't save the image and directly quit the image fater loading <fuelFile>
	
Documentation:
This command will load the <fuelFile> and materialize/install it's contents. If no argument is specified the image continues running with the loaded contents.


Example:

	#Load a fuel file and save and quit the image with the contents:
	pharo Pharo.image save quit path/to/foo.fuel

	#Load the contents of foo.fuel and save the image, but continue running:
	pharo Pharo.image save path/to/foo.fuel
	
	#Load the contents of foo.fuel and continue running without saving:
	pharo Pharo.image path/to/foo.fuel
