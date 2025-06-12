"
I provide commands related to  the image: cleanup and save.
"
Class {
	#name : 'ClapImage',
	#superclass : 'ClapApplication',
	#category : 'Clap-Commands-Pharo',
	#package : 'Clap-Commands-Pharo'
}

{ #category : 'command line - subcommands' }
ClapImage class >> cleanupCommand [

	^ (ClapCommandSpec id: 'cleanup')
		  description: 'Clean caches & other temporary data from the image ';
		  commandClass: self;
		  addFlag: #aggressive description: 'Also delete resources, change sets, etc';
		  addFlag: #release  description: 'Prepare for release (implies --aggressive)';
		  addFlag: #production description: 'Unload tests, examples, etc, for production (implies --release)';
		  yourself
]

{ #category : 'command line' }
ClapImage class >> image [
	<commandline>
	^ (ClapCommandSpec id: #image)
		description: 'Manage image/changes files';
		commandClass: self;
		add: self saveCommand;
		add: self cleanupCommand;
		yourself
]

{ #category : 'command line - subcommands' }
ClapImage class >> saveCommand [

	^ (ClapCommandSpec id: #save)
		  description: 'Save the image/changes file under a new name';
		  commandClass: self;
		  addFlag: #rename;
		  addFlag: #backup;
		  addPositional: #NAME;
		  yourself
]
