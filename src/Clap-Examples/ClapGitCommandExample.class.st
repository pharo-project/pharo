"
Gitd example of Clap commands.

Command specification is defined class side and returns a command specification, which you can then run or just match against an actual invocation (an array of strings, as would be passed by a shell).

Arbitrary command instances can be run as do-its, but pay attention that text printed on the standard output will go nowhere unless the VM was launched from a terminal window:
```
ClapGitCommandLineExample git activateWith: #('git' 'help' 'remote').
```

To inspect the matches without activating the commands, replace #activateWith: with #match:
```
ClapGitCommandLineExample git match: #('git' 'help' 'remote').
```

Commands registered in the system (returned from a class-side method with the <commandline> pragma) can also be invoked from the host system's command line:
```
	$PHARO_VM $IMAGE clap hello Pharo
```
"
Class {
	#name : 'ClapGitCommandExample',
	#superclass : 'ClapApplication',
	#category : 'Clap-Examples',
	#package : 'Clap-Examples'
}

{ #category : 'command line' }
ClapGitCommandExample class >> git [
	"A dummy command (no behavior besides help) to demonstrate nested subcommand and as a test subject for the documenter.
	<commandline> pragma omitted, because doesn't really make sense to make it available outside the image."
	^ (ClapCommandSpec id: 'git')
		description: 'The stupid content tracker';
		commandClass: self;
		add: ClapCommandSpec forHelp;
		add: (ClapGitAddCommandExample specification);
		add: ((ClapCommandSpec id: 'branch')
			description: 'List, create, or delete branches');
		add: ((ClapCommandSpec id: 'clone')
			description: 'Clone a repository into a new directory');
		add: ((ClapCommandSpec id: 'commit')
			description: 'Record changes to the repository');
		add: ((ClapCommandSpec id: 'init')
			description: 'Create an empty Git repository or reinitialize an existing one');
		add: ((ClapCommandSpec id: 'remote')
			description: 'Manage set of tracked repositories';
			add: (ClapCommandSpec id: 'add');
			add: (ClapCommandSpec id: 'remove');
			add: (ClapCommandSpec id: 'set-url');
			yourself);
		add: ((ClapCommandSpec id: 'status')
			description: 'Show the working tree status');
		yourself
]
