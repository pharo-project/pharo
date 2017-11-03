"
Usage: printVersion [ --numeric | --release ]
	--numeric   Print the full version number only (e.g. 12345)
	--release   Print the major relase number only (e.g. 1.2)
	
Documentation:
Prints the version number in an easy to parse format. This can be used in Jenkins with the ""Description Setter"" Plugin. Configure it like this:

Regular expression:  \[version\] (.*)
Description: \1


Examples:
	pharo Pharo.image printVersion
	#result will be something like:
	[version] 3.0 #30100

	pharo Pharo.image printVersion --numeric
	# will print a simpler version
	30100
	
	pharo Pharo.image printVersion --release
	# prints the Pharo release version
	3.0
"
Class {
	#name : #PrintVersionCommandLineHandler,
	#superclass : #CommandLineHandler,
	#category : #System-BasicCommandLineHandler
}

{ #category : #accessing }
PrintVersionCommandLineHandler class >> commandName [
	^ 'printVersion'
]

{ #category : #accessing }
PrintVersionCommandLineHandler class >> description [
	^ 'Print image version'
]

{ #category : #activation }
PrintVersionCommandLineHandler >> activate [
	self activateHelp
		ifTrue: [  ^ self ].
		
	(self hasOption: 'numeric')
		ifTrue: [ 
			self printNumericVersion.
			^ self exitSuccess].
	(self hasOption: 'release')
		ifTrue: [ 
			self printRelease.
			^ self exitSuccess ].
	self hasArguments
		ifFalse: [ 
			self printVersion.
			^ self exitSuccess ].
	
	self printHelp.
	^ self exitFailure
]

{ #category : #actions }
PrintVersionCommandLineHandler >> printNumericVersion [
	self stdout 
		print: SystemVersion current highestUpdate;
		lf
]

{ #category : #actions }
PrintVersionCommandLineHandler >> printRelease [
	self stdout 
		print: SystemVersion current major;
		nextPutAll: '.';
		print: SystemVersion current minor;
		lf.
]

{ #category : #actions }
PrintVersionCommandLineHandler >> printVersion [
	self stdout 
		nextPutAll: '[version] ';
		print: SystemVersion current majorMinorVersion;
		nextPutAll: ' ';
		print: SystemVersion current asString;
		nextPutAll: OSPlatform current lineEnding.
]
