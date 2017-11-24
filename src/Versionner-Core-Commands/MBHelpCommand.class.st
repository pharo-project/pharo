"
Command to open help for Versionner
"
Class {
	#name : #MBHelpCommand,
	#superclass : #MBDevCycleCommands,
	#category : #'Versionner-Core-Commands'
}

{ #category : #documentation }
MBHelpCommand >> documentation [
	^ 'Open the help window'
]

{ #category : #execute }
MBHelpCommand >> execute [
	Smalltalk at: #HelpBrowser ifPresent: [ :class | ^class openOn: (Smalltalk at: #VersionnerHelp) ].
	self workspaceHelp
]

{ #category : #ordering }
MBHelpCommand >> order [

	^'80'
]

{ #category : #accessing }
MBHelpCommand >> title [
	^ 'help'
]

{ #category : #execute }
MBHelpCommand >> workspaceHelp [
	| helpText |
	helpText := '"Versionner is a graphical tool to manage the dependencies and the version of your software. It is a complete replacement of the Monticello browser and acts as a facade for Metacello. You can use Versionner in a number of different scenarios.
	
**1-Creating a new configuration**
Press the button ''+Configuration'' and enter the name of your configuration. An initial baseline will also be produced. Your configuration and the new baseline will be listed on the left hand side of the browser. 

**2-Load a configuration**
The first button of the browser is for loading a configuration from its corresponding SqueakSource project. For example, entering ''Pharo'' will try to load ''ConfigurationOfPharo'' from the ''Pharo'' squeaksource project.

**3-Add a new baseline**
A baseline defines the dependencies of your software, including the list of dependent configurations, and the list of packages that defines your applications. Selecting ''Add Baseline'' by right clicking on an existing baseline will first ask you about your squeaksource username and password will be requested.  You then need to enter the version of your baseline (e.g., 0.1).
The final step is to add the dependent packages. 

**4-Add a new version**
A version defines the version of each packages used by your last baseline. At each change in your source code, you need to create a new version. Pressing ''save all and add development version'' by right clicking in a baseline or version may be the option you want to use. Versionner will try to guess what is the following number you need to use. 
Each unsaved dependent package is then saved. You need to enter a comment for the individual saved package. The configuration itself is then saved. At that stage, all your code is stored in squeaksource.

**5-Reviewing recent changes**
It may be useful to review your change, for example when you want to enter a comment when creating a version. At the bottom of the right hand side panel, you have the list of dependent packages. Modified packages will be designed with a ''*''. Right click on a package to see the difference with the last version stored in the squeaksource project. You can compare a package from the stable version of the software, or from the last commit made on squeaksource.

**6-Load a baseline or a version**
Simply right click on it and press ''load''

**7-Updating a configuration**
In case of concurrent development, you may have to update your configuration. Right-click on the configuration, and press the corresponding menu entry.

**8-Contact**
Feedback are welcome. Send them to the pharo mailing list, or directly to the author alexandrebergel@me.com
"
'.
	self uiManager edit: helpText label: 'Versionner Help'
]
