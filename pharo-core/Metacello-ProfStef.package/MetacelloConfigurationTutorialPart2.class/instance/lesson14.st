lessons
lesson14
	^Lesson
		title: 'Lesson 14'
		lesson: 
'
Smalltalk tools browser fullOnClass: MetacelloProjectRefTutorialConfig selector: #baseline14:.
Smalltalk tools browser fullOnClass: MetacelloProjectRefTutorialConfig selector: #version14:.

"In this lesson we''ll cover the querying API for Metacello. The querying API is useful for analyzing the contents and structure of a version.

To start with we''ll look at version ''1.4'' of the MetacelloProjectRefTutorialConfig. You can list the packages in the version:"

  (MetacelloProjectRefTutorialConfig project version: ''1.4'') packages.

"The list project references:"

  (MetacelloProjectRefTutorialConfig project version: ''1.4'') projects.

"And the groups:"

  (MetacelloProjectRefTutorialConfig project version: ''1.4'') groups.

"You can access individual packages/project refs/groups using  the #packageNamed: method. Here you can access the package named: ''Project-Core'':"

  (MetacelloProjectRefTutorialConfig project version: ''1.4'') packageNamed: ''Project-Core''.

"The project reference named ''Example Default'':"

 (MetacelloProjectRefTutorialConfig project version: ''1.4'') packageNamed: ''Example Default''.

"The group named ''Core'':"

 (MetacelloProjectRefTutorialConfig project version: ''1.4'') packageNamed: ''Core''.

"Each of the attributes of the package can be accessed (#requires, #includes, #file, #repository, #preLoadDoIt, and #postLoadDoit). For example:"

  ((MetacelloProjectRefTutorialConfig project version: ''1.4'') packageNamed: ''Project-Core'') requires.

"Each of the attributes of the project can be accessed (#className, #versionString, #operator, #loads, #file, and #repository). For example:"

  ((MetacelloProjectRefTutorialConfig project version: ''1.4'') packageNamed: ''Example Default'') repository.

"Each of the attributes of the group can be accessed (#includes). For example:"

  ((MetacelloProjectRefTutorialConfig project version: ''1.4'') packageNamed: ''default'') includes.

"When looking at the ''Core'' group, there is only one package listed:"

  (MetacelloProjectRefTutorialConfig project version: ''1.4'') packageNamed: ''Core''.

"In the case of the ''Core'' group, it is defined in terms of the ''default'', which isn''t very useful. When looking at the contents of groups you''d like to see the complete list of packages, without having to explicitly expanding each group you encounter. #packagesForSpecNamed: does just that:"

  (MetacelloProjectRefTutorialConfig project version: ''1.4'') packagesForSpecNamed: ''Core''.

"If you were to load the ''Core'' package:"

  (MetacelloProjectRefTutorialConfig project version: ''1.4'') load: ''Core''.

"You end up seeing the packages from the Example project. If you want to get the list of packages that _would_ be loaded, you can use #allPackagesForSpecNamed:. For example:"

  (MetacelloProjectRefTutorialConfig project version: ''1.4'') allPackagesForSpecNamed: ''Core''.

"If you use #allPackagesForSpecNamed: with a project refernce name, you''ll see the packages that _would_ be loaded:"

  (MetacelloProjectRefTutorialConfig project version: ''1.4'') load: ''Example Default''.
  (MetacelloProjectRefTutorialConfig project version: ''1.4'') allPackagesForSpecNamed: ''Example Default''.

"You can also send #version to a project reference. This is useful if you want to explicitly walk the tree of projects:"

  ((MetacelloProjectRefTutorialConfig project version: ''1.4'') packageNamed: ''Example Default'') version.


ProfStef next.
'