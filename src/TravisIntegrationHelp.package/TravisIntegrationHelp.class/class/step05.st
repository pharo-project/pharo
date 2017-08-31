pages
step05
	^ HelpTopic 
		title: 'Step 05 - Define .travis.yml'
		contents: 
		(self heading: 'Step 05 - Define .travis.yml'),	
		'You need to indicates to Travis on which platform you want to run the tests. Consider the file:', String cr, String cr,
		(self bold: 
'language: smalltalk

os:
  - osx
  - linux

smalltalk:
  - Pharo-6.0
  - Pharo-5.0
'), '
This simple files indicates that the test of your application will be run over a matrix (osx, linux) X (Pharo-6.0, Pharo-5.0).

Travis will begin the build after you have pushed the .travis.yml and .smalltalk.ston files
'