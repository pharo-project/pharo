"
This an example of a subcommand that can be used to define the behaviour of a subcommand, do business validations.
"
Class {
	#name : 'ClapGitAddCommandExample',
	#superclass : 'ClapApplication',
	#category : 'Clap-Examples',
	#package : 'Clap-Examples'
}

{ #category : 'accessing' }
ClapGitAddCommandExample class >> specification [

	^ (ClapCommandSpec id: 'add')
			description: 'Add file contents to the index';
			commandClass: self;
			yourself
]

{ #category : 'execution' }
ClapGitAddCommandExample >> execute [

	self outputStreamDo: [ :str | str nextPutAll: 'Running git add ...' ]
]
