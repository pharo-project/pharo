Class {
	#name : #ClapImage,
	#superclass : #ClapApplication,
	#category : #'Clap-Commands-Pharo'
}

{ #category : #'command line - subcommands' }
ClapImage class >> cleanupCommand [

	^ (ClapCommand id: 'cleanup')
		  description: 'Clean caches & other temporary data from the image ';
		  add: ((ClapFlag id: 'aggressive') description: 'Also delete resources, change sets, etc');
		  add: ((ClapFlag id: 'release') description: 'Prepare for release (implies --aggressive)');
		  add: ((ClapFlag id: 'production') description: 'Unload tests, examples, etc, for production (implies --release)');
		  yourself
]

{ #category : #'command line' }
ClapImage class >> image [
	<commandline>
	^ (ClapCommand id: #image)
		description: 'Manage image/changes files';
		add: self saveCommand;
		add: self cleanupCommand;
		meaning: [ :args | 
			args at: #helpFlag ifPresent: [ :help | 
				help value; exitSuccess ].
			args validateAll.
			(self with: args) execute ]
]

{ #category : #'command line - subcommands' }
ClapImage class >> saveCommand [

	^ (ClapCommand id: #save)
		  description: 'Save the image/changes file under a new name';
		  add: (ClapFlag id: #rename);
		  add: (ClapFlag id: #backup);
		  add: (ClapPositional id: #NAME);
		  yourself
]
