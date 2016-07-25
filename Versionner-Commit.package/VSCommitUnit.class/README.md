I'm a commit unit for a configuration handled with Versionner. 

A commit unit englobates all actions needed to perform a successful commit from a new version. 
Essentially, it will collect all dirty packages and its corresponfing configurations and it will: 

1) commit dirty packages
2) generate new configuration versions
3) commit new configurations

Example:
========
A commit unit can create versions  by working over configurations, so the best way to act is doing something like this: 

"This will create a new major version"
(VSCommitUnit major: ConfigurationOfVersionner) execute.
"This will create a new minor version"
(VSCommitUnit patch: ConfigurationOfVersionner) execute.
"This will create a new patch version"
(VSCommitUnit patch: ConfigurationOfVersionner) execute.