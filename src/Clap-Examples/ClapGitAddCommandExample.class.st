"
This an example of a subcommand that can be used to define the behaviour of a subcommand, do business validations.
"
Class {
	#name : 'ClapGitAddCommandExample',
	#superclass : 'ClapApplication',
	#category : 'Clap-Examples',
	#package : 'Clap-Examples'
}

{ #category : 'execution' }
ClapGitAddCommandExample >> execute [

	self outputStreamDo: [ :str | str nextPutAll: 'Running git add ...' ]
]
